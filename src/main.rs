use ignore::types::TypesBuilder;
use ignore::WalkBuilder;
use std::borrow::Cow;
use std::collections::HashMap;
use std::env;
use std::sync::Mutex;

use fern::colors::{Color, ColoredLevelConfig};
#[allow(unused_imports)]
use log::{debug, error, info, trace, warn};

mod default_types;

/// Get the filename of a path, with backslashes replaced with forward slashes
fn unixy_filename_of(path: &std::path::Path) -> String {
    return path.display().to_string().replace("\\", "/");
}

fn set_up_logging() {
    let colors_level = ColoredLevelConfig::new()
        .error(Color::Red)
        .warn(Color::Yellow)
        .info(Color::Blue)
        .debug(Color::Cyan);

    println!("{}", env!("CARGO_PKG_NAME").replace("-", "_"));

    fern::Dispatch::new()
        .format(move |out, message, record| {
            out.finish(format_args!(
                "{date} {target} {level} {message}",
                date = chrono::Local::now().format("%Y-%m-%d %H:%M:%S"),
                target = record.target(),
                level = colors_level.color(record.level()),
                message = message,
            ));
        })
        .level(log::LevelFilter::Warn)
        .level_for(
            env!("CARGO_PKG_NAME").replace("-", "_"),
            log::LevelFilter::Debug,
        )
        .chain(std::io::stderr())
        .apply()
        .unwrap();

    debug!("finished setting up logging! yay!");
}

#[derive(Debug, Clone)]
struct Analysis {
    pub char_counts: HashMap<char, i32>,
    pub bigrams: HashMap<(char, char), i32>,
    pub trigrams: HashMap<(char, char, char), i32>,
}

impl Analysis {
    pub fn new() -> Analysis {
        Analysis {
            char_counts: HashMap::new(),
            bigrams: HashMap::new(),
            trigrams: HashMap::new(),
        }
    }

    pub fn record_char(&mut self, c: char, occurrences: i32) {
        Self::increment_count(c, occurrences, &mut self.char_counts);
    }

    pub fn record_bigram(&mut self, bigram: (char, char), occurrences: i32) {
        Self::increment_count(bigram, occurrences, &mut self.bigrams);
    }

    pub fn record_trigram(&mut self, trigram: (char, char, char), occurrences: i32) {
        Self::increment_count(trigram, occurrences, &mut self.trigrams);
    }

    pub fn incorporate(&mut self, other: &Self) {
        for (k, count) in other.char_counts.iter() {
            self.record_char(*k, *count);
        }
        for (k, count) in other.bigrams.iter() {
            self.record_bigram(*k, *count);
        }
        for (k, count) in other.trigrams.iter() {
            self.record_trigram(*k, *count);
        }
    }

    pub fn is_empty(&self) -> bool {
        self.char_counts.is_empty()
    }

    fn increment_count<E: std::cmp::Eq + std::hash::Hash>(
        x: E,
        extra_count: i32,
        map: &mut HashMap<E, i32>,
    ) {
        if let Some(count) = map.get_mut(&x) {
            *count += extra_count;
        } else {
            map.insert(x, extra_count);
        }
    }
}

fn is_untypeable_char(c: char) -> bool {
    c == '\r'
}

fn analyze_file(contents: &str) -> Analysis {
    let mut r = Analysis::new();

    // a, b, and c are the last 2 chars plus the current one; can't use Rust's iter::window() when
    // we want to get `char`s rather than bytes, because we can't get a slice of bytes from a str
    let mut a = None;
    let mut b = None;
    for mut c in contents.chars() {
        if is_untypeable_char(c) {
            trace!("dropping untypable char: {:?}", c);
            continue;
        }

        // lowercase characters are significantly more common in most programming languages (and
        // natural languages), so it makes sense to normalize to lowercase rather than uppercase
        if c.is_ascii_uppercase() {
            c = c.to_ascii_lowercase();
        }

        r.record_char(c, 1);
        if let Some(b) = b {
            r.record_bigram((b, c), 1);
            if let Some(a) = a {
                r.record_trigram((a, b, c), 1);
            }
        }

        a = b;
        b = Some(c);
    }

    r
}

#[derive(Debug, Clone)]
struct Lang {
    name: String,
    globs: Vec<String>,
}

impl Lang {
    pub fn new(name: &str, globs: &[String]) -> Lang {
        Lang {
            name: name.to_owned(),
            globs: globs.to_vec(),
        }
    }
}

impl std::hash::Hash for Lang {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl PartialEq for Lang {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
impl Eq for Lang {}

#[derive(Debug)]
struct Analyzer<'l> {
    types: ignore::types::Types,
    langs: &'l Vec<Lang>,
    dir_results: HashMap<Lang, Analysis>,
    lang_name_to_lang: HashMap<&'l str, &'l Lang>,
    final_results: &'l Mutex<HashMap<Lang, Analysis>>,
}

impl ignore::ParallelVisitor for Analyzer<'_> {
    fn visit(&mut self, result: Result<ignore::DirEntry, ignore::Error>) -> ignore::WalkState {
        let direntry = result.expect("valid direntry");
        let filename = unixy_filename_of(&direntry.path());
        let is_dir = direntry.file_type().expect("valid file info").is_dir();
        if is_dir {
            trace!("now analysing files in directory {}", filename);
        } else {
            let mat = self.types.matched(
                direntry.path(),
                direntry.file_type().expect("valid file info").is_dir(),
            );

            if let Some(lang_name) = mat
                .inner()
                .and_then(|glob| glob.file_type_def())
                .map(|ftd| ftd.name())
            {
                let lang = self.lang_name_to_lang[lang_name];

                trace!("now analysing {}", &filename);
                match std::fs::read_to_string(direntry.path()) {
                    Ok(file_contents) => {
                        let file_results = analyze_file(file_contents.as_str());
                        if let Some(lang_analysis) = self.dir_results.get_mut(&lang) {
                            lang_analysis.incorporate(&file_results);
                        }
                    }
                    Err(err) => {
                        let tip = if filename.len() > 260 {
                            // path is longer than normal windows limit - long path support might need to be enabled
                            Some(Cow::from(format!(". NB: path length of {len} is greater than 260 limit; you might need to enable the 'Enable Win32 long paths' group policy setting", len = filename.len())))
                        } else {
                            None
                        };
                        error!(
                            "Error failed to analyse file {path}: {err:?}{tip}",
                            err = err,
                            tip = tip.unwrap_or(Cow::from("")),
                            path = filename
                        );
                    }
                }
            } else {
                warn!(
                    "Received file that does not match type filter! {}",
                    filename
                )
            }
        }

        ignore::WalkState::Continue
    }
}

impl<'l> Drop for Analyzer<'l> {
    fn drop(&mut self) {
        trace!("Dropping analyzer and combining results");
        let lock_result = self.final_results.lock();
        if let Ok(mut results_guard) = lock_result {
            for (lang, results) in results_guard.iter_mut() {
                results.incorporate(&self.dir_results[&lang]);
            }
        } else {
            panic!("analyzer sees that mutex for collating results has been poisoned")
        }
    }
}

struct AnalyzerBuilder<'l> {
    analyzer_prototype: Analyzer<'l>,
}

impl AnalyzerBuilder<'_> {
    pub fn new<'l>(
        langs: &'l Vec<Lang>,
        types: &ignore::types::Types,
        final_results: &'l Mutex<HashMap<Lang, Analysis>>,
    ) -> AnalyzerBuilder<'l> {
        AnalyzerBuilder {
            analyzer_prototype: Analyzer {
                types: types.clone(),
                langs: langs,
                lang_name_to_lang: langs
                    .iter()
                    .map(|lang| (lang.name.as_str(), lang))
                    .collect::<HashMap<_, _>>(),
                dir_results: langs
                    .clone()
                    .into_iter()
                    .map(|lang| (lang, Analysis::new()))
                    .collect(),
                final_results: final_results,
            },
        }
    }
}

impl<'l> ignore::ParallelVisitorBuilder<'l> for AnalyzerBuilder<'l> {
    fn build(&mut self) -> Box<dyn ignore::ParallelVisitor + 'l> {
        trace!("Building new analyzer");
        Box::new(Analyzer {
            dir_results: self.analyzer_prototype.dir_results.clone(),
            types: self.analyzer_prototype.types.clone(),
            lang_name_to_lang: self.analyzer_prototype.lang_name_to_lang.clone(),
            langs: self.analyzer_prototype.langs,
            final_results: self.analyzer_prototype.final_results,
        })
    }
}

fn analyze_dir(langs: &Vec<Lang>, dir: &str) -> HashMap<Lang, Analysis> {
    let mut tb = TypesBuilder::new();
    for lang in langs {
        for glob in &lang.globs {
            tb.add(&lang.name, &glob)
                .expect("adding type should not fail");
            tb.select(&lang.name);
        }
    }
    let types = tb
        .build()
        .expect("type builder construction shouldn't fail");

    let dir_results: HashMap<Lang, Analysis> = langs
        .clone()
        .into_iter()
        .map(|lang| (lang, Analysis::new()))
        .collect();

    let final_results = std::sync::Mutex::new(dir_results.clone());

    let mut analyzer_builder = AnalyzerBuilder::new(&langs, &types, &final_results);

    WalkBuilder::new(dir)
        .types(types.clone())
        .build_parallel()
        .visit(&mut analyzer_builder);

    let lock_result = final_results.lock();
    if let Ok(results_guard) = lock_result {
        return results_guard.clone();
    } else {
        panic!("main thread sees that mutex for collating results has been poisoned")
    }
}

fn char_name(c: char) -> String {
    match c {
        ' ' => "space".to_owned(),
        '\t' => "tab".to_owned(),
        '\n' => "enter".to_owned(),
        _ => format!("{}", c),
    }
}

fn format_results(analysis: &Analysis) -> String {
    let c_total = analysis.char_counts.values().sum::<i32>();
    let bi_total = analysis.bigrams.values().sum::<i32>();
    let tri_total = analysis.trigrams.values().sum::<i32>();

    let mut s = format!(
        "Characters: {c_total} total, {c_uniq} unique (case insensitive)
Bigrams: {bi_total} total, {bi_uniq} unique
Trigrams: {tri_total} total, {tri_uniq} unique
",
        c_total = c_total,
        c_uniq = analysis.char_counts.len(),
        bi_total = bi_total,
        bi_uniq = analysis.bigrams.len(),
        tri_total = tri_total,
        tri_uniq = analysis.trigrams.len(),
    );

    let mut sorted: Vec<_> = analysis
        .char_counts
        .iter()
        .map(|(c, count)| (c, count, *count as f32 * 100.0 / c_total as f32))
        .collect::<Vec<_>>();
    sorted.sort_by_key(|(_c, count, ..)| -*count);

    for (c, count, perc) in sorted.iter() {
        s += format!(
            "{c:>5} {count:>8}  {perc:05.2}%\n",
            c = char_name(**c),
            count = count,
            perc = perc
        )
        .as_str();
    }

    s
}

fn main() {
    set_up_logging();

    let args: Vec<_> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Error: expecting exactly 1 argument");
        eprintln!("Usage: {} DIRECTORY", args[0]);
        eprintln!("\n  DIRECTORY   Root directory to start examining files from");
    }
    let root = args.get(1).expect("first argument must be path");

    let langs = default_types::DEFAULT_TYPES
        .iter()
        .map(|(filetype, exts)| {
            let es = exts.iter().map(|e| e.to_string()).collect::<Vec<_>>();
            Lang::new(filetype, es.as_ref())
        })
        .collect();

    let results = analyze_dir(&langs, root);

    //TODO display aggregate stats for all languages combined
    trace!("Analysis complete - now showing results");
    for (lang, analysis) in results.iter().filter(|(_l, a)| !a.is_empty()) {
        println!("Language: {}", lang.name);

        let r = format_results(&analysis);
        for line in r.lines() {
            println!("\t{}", line);
        }
    }
}

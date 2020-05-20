use regex::Regex;
use std::borrow::Cow;
use std::collections::HashMap;
use std::env;
use walkdir::WalkDir;

use fern::colors::{Color, ColoredLevelConfig};
#[allow(unused_imports)]
use log::{debug, error, info, trace, warn};

/// Get the filename of an entry from a directory walk, with backslashes replaced with forward slashes
fn unixy_filename_of(entry: &walkdir::DirEntry) -> String {
    return entry.path().display().to_string().replace("\\", "/");
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
            log::LevelFilter::Trace,
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
    patterns: Vec<Regex>,
}

impl Lang {
    pub fn new(name: &str, patterns: Vec<&str>) -> Lang {
        Lang {
            name: name.to_owned(),
            patterns: patterns
                .iter()
                .map(|p| Regex::new(p).expect("lang pattern must be valid"))
                .collect(),
        }
    }

    pub fn matches_file(&self, filename: &str) -> bool {
        for pattern in self.patterns.iter() {
            if pattern.is_match(filename) {
                return true;
            }
        }
        false
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

fn analyze_dir(langs: &Vec<Lang>, dir: &str) -> HashMap<Lang, Analysis> {
    let mut dir_results: HashMap<Lang, Analysis> = langs
        .clone()
        .into_iter()
        .map(|lang| (lang, Analysis::new()))
        .collect();

    for (lang, filename, entry) in WalkDir::new(dir)
        .same_file_system(true)
        .into_iter()
        .map(|maybe_entry| maybe_entry.expect("able to read file"))
        .filter_map(|entry| {
            if !entry.file_type().is_file() {
                return None;
            }
            let filename = unixy_filename_of(&entry);

            // TODO: use the `ignore` crate instead and replace this with a Types specification
            // https://docs.rs/ignore/0.4.15/ignore/types/struct.TypesBuilder.html
            // (maybe also make the iteration happen in parallel while we're at it)
            let matching_langs: Vec<&Lang> = langs
                .iter()
                .filter(|lang| lang.matches_file(filename.as_ref()))
                .collect();
            if matching_langs.len() > 1 {
                let err_msg = format!(
                    "Multiple languages are configured to match the file: {}",
                    filename
                );
                panic!(err_msg);
            } else {
                matching_langs
                    .first()
                    .map(|lang| (lang.clone(), filename, entry))
            }
        })
    {
        trace!("analysing {}", &filename);
        match std::fs::read_to_string(entry.path()) {
            Ok(file_contents) => {
                let file_results = analyze_file(file_contents.as_str());
                if let Some(lang_analysis) = dir_results.get_mut(lang) {
                    lang_analysis.incorporate(&file_results);
                }
            }
            Err(err) => {
                let tip = if filename.len() > 260 {
                    // path is longer than normal windows limit - long path support might need to be enabled
                    Some(Cow::from(format!("NB: path length of {len} is greater than 260 limit; you might need to enable the 'Enable Win32 long paths' group policy setting", len = filename.len())))
                } else {
                    None
                };
                eprintln!(
                    "Error:{err:?}{tip}  {path}",
                    err = err,
                    tip = tip.unwrap_or(Cow::from("")),
                    path = filename
                );
            }
        }
    }

    dir_results
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

    //TODO add more languages
    // https://github.com/BurntSushi/ripgrep/blob/master/crates/ignore/src/default_types.rs
    // has a nice mapping
    let langs = vec![Lang::new("rust", vec![r".*\.rs$"])];

    let results = analyze_dir(&langs, root);

    for (lang, analysis) in results.iter() {
        println!("Language: {}", lang.name);

        let r = format_results(&analysis);
        for line in r.lines() {
            println!("\t{}", line);
        }
    }
}

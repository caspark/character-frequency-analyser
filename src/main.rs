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
}

impl Analysis {
    pub fn new() -> Analysis {
        Analysis {
            char_counts: HashMap::with_capacity(64), // 64 slots should be enough for the most common characters
        }
    }

    fn record_char(&mut self, c: char, occurrences: i32) {
        if let Some(count) = self.char_counts.get_mut(&c) {
            *count += occurrences;
        } else {
            self.char_counts.insert(c, occurrences);
        }
    }

    fn incorporate(&mut self, other: &Self) {
        for (c, count) in other.char_counts.iter() {
            self.record_char(*c, *count);
        }
    }
}

fn analyze_file(contents: &str) -> Analysis {
    let mut r = Analysis::new();

    for c in contents.chars() {
        r.record_char(c, 1);
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
    let langs = vec![Lang::new("rust", vec![r".*\.rs$"])];

    let results = analyze_dir(&langs, root);

    //TODO better result formatting
    println!("Results:\n{:?}", results);
}

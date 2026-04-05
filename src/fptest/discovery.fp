use std::fs;
use std::path::Path;
use std::option::Option;
use fptest::config::Config;
use fptest::markers;

pub struct TestFile {
    path: str,
    markers: markers::MarkerSet,
}

fn matches_name(name: &str) -> bool {
    (name.starts_with("test_") && name.ends_with(".fp"))
        || name.ends_with("_test.fp")
}

fn matches_keyword(keyword: &str, path: &str, name: &str) -> bool {
    if keyword == "" {
        return true;
    }
    path.contains(keyword) || name.contains(keyword)
}

pub fn discover(config: &Config) -> Vec<TestFile> {
    let mut files: Vec<TestFile> = Vec::new();
    let mut paths: Vec<&str> = Vec::new();

    if config.pattern() != "" {
        paths = fs::glob(config.pattern());
    } else {
        paths = fs::walk_dir(&Path::new(config.root()));
    }

    let mut idx = 0;
    while idx < paths.len() {
        let path = paths[idx];
        let path_obj = Path::new(path);
        match path_obj.extension() {
            Option::Some(ext) => {
                if ext != "fp" {
                    idx = idx + 1;
                    continue;
                }
            }
            Option::None => {
                idx = idx + 1;
                continue;
            }
        }

        let name = match path_obj.file_name() {
            Option::Some(file_name) => file_name,
            Option::None => {
                idx = idx + 1;
                continue;
            }
        };

        if !matches_name(name) {
            idx = idx + 1;
            continue;
        }

        if !matches_keyword(config.keyword(), path, name) {
            idx = idx + 1;
            continue;
        }

        let markers = markers::parse_markers(path);
        files.push(TestFile {
            path,
            markers,
        });

        idx = idx + 1;
    }

    files
}

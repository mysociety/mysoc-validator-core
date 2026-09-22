//! File discovery and per-file validation; results are sorted by path.

use crate::{Transcript, TranscriptError, TranscriptFileResult};
use std::collections::HashSet;
use std::fs;
use std::path::Path;

impl Transcript {
    pub fn from_xml_path(path: impl AsRef<Path>) -> Result<Self, Vec<TranscriptError>> {
        let path = path.as_ref();
        let xml = fs::read_to_string(path).map_err(|error| {
            vec![TranscriptError {
                location: path.display().to_string(),
                message: error.to_string(),
            }]
        })?;
        Self::model_validate_xml(&xml)
    }
}
pub fn validate_folder(
    folder: impl AsRef<Path>,
    person_ids: Option<&HashSet<String>>,
) -> Result<Vec<TranscriptFileResult>, std::io::Error> {
    let mut paths = Vec::new();
    collect_xml_paths(folder.as_ref(), &mut paths)?;
    paths.sort();
    Ok(paths
        .into_iter()
        .map(|path| {
            let errors = match Transcript::from_xml_path(&path) {
                Ok(transcript) => person_ids
                    .map(|ids| transcript.validate_person_references(ids))
                    .unwrap_or_default(),
                Err(errors) => errors,
            };
            TranscriptFileResult { path, errors }
        })
        .collect())
}

fn collect_xml_paths(folder: &Path, paths: &mut Vec<std::path::PathBuf>) -> std::io::Result<()> {
    for entry in fs::read_dir(folder)? {
        let entry = entry?;
        let file_type = entry.file_type()?;
        if file_type.is_dir() {
            collect_xml_paths(&entry.path(), paths)?;
        } else if file_type.is_file()
            && entry
                .path()
                .extension()
                .is_some_and(|extension| extension == "xml")
        {
            paths.push(entry.path());
        }
    }
    Ok(())
}

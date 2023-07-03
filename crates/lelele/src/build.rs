//! Build script support.

use crate::{codegen::Codegen, dfa::DFA, grammar::Grammar};
use anyhow::Context as _;
use std::{
    env, fs,
    path::{Path, PathBuf},
};
use walkdir::WalkDir;

pub fn process_root() -> anyhow::Result<()> {
    let build = Build::new()?;
    build.process()
}

pub fn process_dir(root_dir: &Path) -> anyhow::Result<()> {
    let build = Build::with_root_dir(root_dir.to_owned())?;
    build.process()
}

#[derive(Debug)]
pub struct Build {
    root_dir: PathBuf,
    out_dir: PathBuf,
}

impl Build {
    pub fn new() -> anyhow::Result<Self> {
        let root_dir = env::var_os("CARGO_MANIFEST_DIR")
            .map(PathBuf::from)
            .context("The environment variable `CARGO_MANIFEST_DIR' is not set")?;
        Self::with_root_dir(root_dir)
    }

    pub fn with_root_dir(root_dir: PathBuf) -> anyhow::Result<Self> {
        let out_dir = env::var_os("OUT_DIR")
            .map(PathBuf::from)
            .context("The environment variable `OUT_DIR' is not set")?;
        Ok(Self { root_dir, out_dir })
    }

    pub fn process(&self) -> anyhow::Result<()> {
        for entry in WalkDir::new(&self.root_dir) {
            let entry = entry.context("from WalkDir entry")?;
            if !entry.file_type().is_file() {
                continue;
            }
            let in_file = entry.path();
            match in_file.extension().and_then(|ext| ext.to_str()) {
                Some("lll") => self.process_file(in_file)?,
                _ => continue,
            }
        }

        Ok(())
    }

    fn process_file(&self, in_file: &Path) -> anyhow::Result<()> {
        let mut expanded_file = in_file.to_path_buf();
        expanded_file.set_extension("lll.expanded");

        let mut automaton_file = in_file.to_path_buf();
        automaton_file.set_extension("lll.automaton");

        let mut out_file = self.out_dir.join(in_file.strip_prefix(&self.root_dir)?);
        out_file.set_extension("rs");
        if let Some(out_dir) = out_file.parent() {
            fs::create_dir_all(&out_dir)?;
        }

        println!("cargo:rerun-if-changed={}", in_file.display());

        let grammar = Grammar::from_file(&in_file)?;
        let dfa = DFA::generate(&grammar);
        let codegen = Codegen::new(&grammar, &dfa);

        fs::write(&out_file, codegen.to_string())?;
        fs::write(&expanded_file, grammar.to_string())?;
        fs::write(&automaton_file, dfa.display(&grammar).to_string())?;

        Ok(())
    }
}

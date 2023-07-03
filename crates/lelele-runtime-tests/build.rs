use std::{env, path::PathBuf};

fn main() {
    let project_root = env::var_os("CARGO_MANIFEST_DIR")
        .map(PathBuf::from)
        .unwrap();
    let test_grammars_dir = project_root.join("../lelele/tests").canonicalize().unwrap();
    lelele::build::process_dir(&test_grammars_dir).unwrap();
}

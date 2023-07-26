use std::{env, path::PathBuf};

macro_rules! define_tests {
    ($($name:ident),*$(,)?) => {$(
        #[test]
        fn $name() {
            let grammar = lelele::syntax::parse_file(
                &PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap())
                    .join(concat!("tests/", stringify!($name), ".lll"))
            ).unwrap();
            let _table = lelele::ielr::compute(&grammar.cfg, Default::default()).unwrap();
        }
    )*};
}

define_tests! {
    arithmetic,
    arithmetic_prec,
    g1,
    g2,
    g4,
    g5,
    g6,
    g7,
    g8,
    g9,
    g10,
}

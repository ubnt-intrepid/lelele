use lelele::{dfa::Config, grammar::Grammar};
use std::{env, path::PathBuf};

macro_rules! define_tests {
    ($($name:ident),*$(,)?) => {$(
        #[test]
        fn $name() {
            let grammar = Grammar::from_file(
                &PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap())
                    .join(concat!("tests/", stringify!($name), ".lll"))
            ).unwrap();
            eprintln!("grammar:\n{}", grammar);
            eprintln!();

            eprintln!("DFA(canonical):");
            let dfa = Config::new().use_canonical().generate(&grammar);
            eprintln!("num_nodes: {}", dfa.nodes().count());
            eprintln!("---\n{}\n", dfa.display(&grammar));
            eprintln!();

            eprintln!("DFA(PGM):");
            let dfa = Config::new().use_pgm().generate(&grammar);
            eprintln!("num_nodes: {}", dfa.nodes().count());
            eprintln!("---\n{}\n", dfa.display(&grammar));
            eprintln!();

            eprintln!("DFA(LALR):");
            let dfa = Config::new().use_lalr().generate(&grammar);
            eprintln!("num_nodes: {}", dfa.nodes().count());
            eprintln!("---\n{}\n", dfa.display(&grammar));
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

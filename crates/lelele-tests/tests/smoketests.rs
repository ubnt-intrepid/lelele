use lelele::{
    dfa::Config,
    grammar::{Grammar, GrammarDef},
};
use lelele_tests::grammars;

fn smoketest_grammar(f: impl FnOnce(&mut GrammarDef<'static>)) {
    let grammar = Grammar::define(f);
    eprintln!("grammar:\n{}", grammar);
    eprintln!();

    eprintln!("DFA(canonical):");
    let dfa = Config::new(&grammar).generate();
    eprintln!("num_nodes: {}", dfa.nodes().count());
    eprintln!("---\n{}\n", dfa);
    eprintln!();

    eprintln!("DFA(LALR):");
    let dfa = Config::new(&grammar).use_lalr().generate();
    eprintln!("num_nodes: {}", dfa.nodes().count());
    eprintln!("---\n{}\n", dfa);
}

#[test]
fn smoketest_g_simple1() {
    smoketest_grammar(grammars::g_simple1);
}

#[test]
fn smoketest_g_simple2() {
    smoketest_grammar(grammars::g_simple2);
}

#[test]
fn smoketest_g1() {
    smoketest_grammar(grammars::g1);
}

#[test]
fn smoketest_g2() {
    smoketest_grammar(grammars::g2);
}

#[test]
fn smoketest_g4() {
    smoketest_grammar(grammars::g4);
}

#[test]
fn smoketest_min_caml() {
    smoketest_grammar(grammars::min_caml);
}

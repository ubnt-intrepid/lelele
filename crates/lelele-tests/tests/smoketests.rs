use lelele::{
    dfa::Config,
    grammar::{Grammar, GrammarDef},
};
use lelele_tests::grammars;

fn smoketest_grammar(f: impl FnOnce(&mut GrammarDef<'_>)) {
    let grammar = Grammar::define(f);
    eprintln!("grammar:\n{}", grammar);
    eprintln!();

    eprintln!("DFA(canonical):");
    let dfa = Config::new().use_canonical().generate(&grammar);
    eprintln!("num_nodes: {}", dfa.nodes().count());
    eprintln!("---\n{}\n", dfa);
    eprintln!();

    eprintln!("DFA(PGM):");
    let dfa = Config::new().use_pgm().generate(&grammar);
    eprintln!("num_nodes: {}", dfa.nodes().count());
    eprintln!("---\n{}\n", dfa);
    eprintln!();

    eprintln!("DFA(LALR):");
    let dfa = Config::new().use_lalr().generate(&grammar);
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
fn smoketest_g5() {
    smoketest_grammar(grammars::g5);
}

#[test]
fn smoketest_g6() {
    smoketest_grammar(grammars::g6);
}

#[test]
fn smoketest_g7() {
    smoketest_grammar(grammars::g7);
}

#[test]
fn smoketest_g8() {
    smoketest_grammar(grammars::g8);
}

#[test]
fn smoketest_g9() {
    smoketest_grammar(grammars::g9);
}

#[test]
fn smoketest_g10() {
    smoketest_grammar(grammars::g10);
}

#[test]
fn smoketest_min_caml() {
    smoketest_grammar(grammars::min_caml);
}

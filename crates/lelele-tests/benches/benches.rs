use criterion::{criterion_group, criterion_main, Criterion};
use lelele::{
    dfa::DFA,
    grammar::{Grammar, GrammarDef},
};
use lelele_tests::grammars;
use std::hint::black_box;

criterion_main!(benches);
criterion_group!(benches, bench_simple_1, bench_simple_2, bench_min_caml);

fn bench_simple_1(c: &mut Criterion) {
    bench_dfa_gen(c, "g_simple1", grammars::g_simple1);
    bench_dfa_gen(c, "g_simple2", grammars::g_simple2);
}

fn bench_simple_2(c: &mut Criterion) {
    bench_dfa_gen(c, "g1", grammars::g1);
    bench_dfa_gen(c, "g2", grammars::g2);
    bench_dfa_gen(c, "g4", grammars::g4);
}

fn bench_min_caml(c: &mut Criterion) {
    bench_dfa_gen(c, "min_caml", grammars::min_caml);
}

fn bench_dfa_gen(c: &mut Criterion, name: &str, f: impl FnOnce(&mut GrammarDef<'static>)) {
    let grammar = Grammar::define(f);
    c.bench_function(name, |b| {
        b.iter(|| {
            let _dfa = black_box(DFA::generate(&grammar));
        });
    });
}

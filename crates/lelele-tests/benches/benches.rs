use criterion::{criterion_group, criterion_main, Criterion};
use lelele::{
    dfa::Config,
    grammar::{Grammar, GrammarDef},
};
use lelele_tests::grammars;

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
    bench_dfa_gen(c, "MinCaml", grammars::min_caml);
}

fn bench_dfa_gen(c: &mut Criterion, group_name: &str, f: impl FnOnce(&mut GrammarDef<'_>)) {
    let mut group = c.benchmark_group(group_name);
    let grammar = Grammar::define(f);
    group.bench_function("Canonical", |b| {
        b.iter(|| Config::new().use_canonical().generate(&grammar));
    });
    group.bench_function("PGM", |b| {
        b.iter(|| Config::new().use_pgm().generate(&grammar));
    });
    group.bench_function("LALR", |b| {
        b.iter(|| Config::new().use_lalr().generate(&grammar));
    });
    group.finish();
}

use criterion::{criterion_group, criterion_main, Criterion};
use lelele::{
    dfa::Config,
    grammar::{Grammar, GrammarDef, GrammarDefError},
};
use lelele_tests::grammars;

criterion_main!(benches);
criterion_group!(benches, bench_arithmetic, bench_simple_2);

fn bench_arithmetic(c: &mut Criterion) {
    bench_dfa_gen(c, "g_arithmetic", grammars::g_arithmetic);
    bench_dfa_gen(c, "g_arithmetic_prec", grammars::g_arithmetic_prec);
}

fn bench_simple_2(c: &mut Criterion) {
    bench_dfa_gen(c, "g1", grammars::g1);
    bench_dfa_gen(c, "g2", grammars::g2);
    bench_dfa_gen(c, "g4", grammars::g4);
    bench_dfa_gen(c, "g5", grammars::g5);
    bench_dfa_gen(c, "g6", grammars::g6);
    bench_dfa_gen(c, "g7", grammars::g7);
    bench_dfa_gen(c, "g8", grammars::g8);
    bench_dfa_gen(c, "g9", grammars::g9);
    bench_dfa_gen(c, "g10", grammars::g10);
}

fn bench_dfa_gen(
    c: &mut Criterion,
    group_name: &str,
    f: impl FnOnce(&mut GrammarDef<'_>) -> Result<(), GrammarDefError>,
) {
    let mut group = c.benchmark_group(group_name);
    let grammar = Grammar::define(f).unwrap();
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

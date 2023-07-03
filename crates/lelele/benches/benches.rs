use std::{env, path::PathBuf};

use criterion::{criterion_group, criterion_main, Criterion};
use lelele::{dfa::Config, grammar::Grammar};

criterion_main!(benches);
criterion_group!(benches, bench_arithmetic, bench_simple_2);

fn bench_arithmetic(c: &mut Criterion) {
    bench_dfa_gen(c, "arithmetic");
    bench_dfa_gen(c, "arithmetic_prec");
}

fn bench_simple_2(c: &mut Criterion) {
    bench_dfa_gen(c, "g1");
    bench_dfa_gen(c, "g2");
    bench_dfa_gen(c, "g4");
    bench_dfa_gen(c, "g5");
    bench_dfa_gen(c, "g6");
    bench_dfa_gen(c, "g7");
    bench_dfa_gen(c, "g8");
    bench_dfa_gen(c, "g9");
    bench_dfa_gen(c, "g10");
}

fn bench_dfa_gen(c: &mut Criterion, grammar_name: &str) {
    let project_root = env::var_os("CARGO_MANIFEST_DIR")
        .map(PathBuf::from)
        .expect("missing environment variable: `CARGO_MANIFEST_DIR'");
    let grammar =
        Grammar::from_file(&project_root.join(format!("tests/{}.lll", grammar_name))).unwrap();

    let mut group = c.benchmark_group(grammar_name);
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

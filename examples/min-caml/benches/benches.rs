use std::{env, path::PathBuf};

use criterion::{criterion_group, criterion_main, Criterion};
use lelele::{
    grammar::Grammar,
    lr1::{Config, DFA},
};

criterion_main!(benches);
criterion_group!(benches, bench_grammar_gen);

fn bench_grammar_gen(c: &mut Criterion) {
    let project_root = env::var_os("CARGO_MANIFEST_DIR")
        .map(PathBuf::from)
        .expect("missing environment variable: `CARGO_MANIFEST_DIR'");
    let grammar = Grammar::from_file(&project_root.join("min_caml.lll")).unwrap();

    let mut group = c.benchmark_group("MinCaml");
    group.bench_function("Canonical", |b| {
        b.iter(|| DFA::generate_with_config(&grammar, Config::new().use_canonical()));
    });
    group.bench_function("PGM", |b| {
        b.iter(|| DFA::generate_with_config(&grammar, Config::new().use_pgm()));
    });
    group.bench_function("LALR", |b| {
        b.iter(|| DFA::generate_with_config(&grammar, Config::new().use_lalr()));
    });
    group.finish();
}

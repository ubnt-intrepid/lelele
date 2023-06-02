use criterion::{criterion_group, criterion_main, Criterion};
use lelele::{
    dfa::DFA,
    grammar::{Choice, Grammar, GrammarDef},
};
use std::hint::black_box;

criterion_main!(benches);
criterion_group!(benches, bench_g1, bench_g2);

fn bench_g1(c: &mut Criterion) {
    let grammar = Grammar::define(g1);
    c.bench_function("g1", |b| {
        b.iter(|| {
            let _dfa = black_box(DFA::generate(&grammar));
        })
    });
}

fn bench_g2(c: &mut Criterion) {
    let grammar = Grammar::define(g2);
    c.bench_function("g2", |b| {
        b.iter(|| {
            let _dfa = black_box(DFA::generate(&grammar));
        })
    });
}

fn g1(def: &mut GrammarDef<'_>) {
    let equal = def.token("EQUAL");
    let plus = def.token("PLUS");
    let ident = def.token("ID");
    let num = def.token("NUM");

    let a = def.symbol("A");
    let e = def.symbol("E");
    let t = def.symbol("T");

    def.start_symbol(a);

    def.rule(a, Choice(((e, equal, e), ident)));
    def.rule(e, Choice(((e, plus, t), t)));
    def.rule(t, Choice((num, ident)));
}

fn g2(def: &mut GrammarDef<'_>) {
    // declare terminal symbols.
    let lparen = def.token("LPAREN");
    let rparen = def.token("RPAREN");
    let plus = def.token("PLUS");
    let minus = def.token("MINUS");
    let star = def.token("STAR");
    let slash = def.token("SLASH");
    let num = def.token("NUM");
    let _ = def.token("UNUSED_0");

    // declare nonterminal symbols.
    let expr = def.symbol("EXPR");
    let factor = def.symbol("FACTOR");
    let term = def.symbol("TERM");
    let _ = def.symbol("UNUSED_1");

    def.start_symbol(expr);

    // declare syntax rules.
    def.rule(
        expr,
        Choice((
            (expr, plus, factor),  // expr '+' factor
            (expr, minus, factor), // expr '-' factor
            factor,                // factor
        )),
    );
    def.rule(
        factor,
        Choice((
            (factor, star, term),  // factor '*' term
            (factor, slash, term), // factor '/' term
            term,                  // term
        )),
    );
    def.rule(
        term,
        Choice((
            num,                    // num
            (lparen, expr, rparen), // '(' expr ')'
        )),
    );
}

fn main() {
    let mut builder = lelele::Grammar::builder();
    builder
        .terminals(&["NUM", "LPAREN", "RPAREN", "PLUS", "0", "1"])
        .rule("EXPR", &["FACTOR"])
        .rule("EXPR", &["LPAREN", "EXPR", "RPAREN"])
        .rule("FACTOR", &["NUM"])
        .rule("FACTOR", &["PLUS", "FACTOR"])
        .rule("FACTOR", &["FACTOR", "PLUS", "NUM"])
        .rule("X", &["Y", "0"])
        .rule("Y", &["1"])
        .rule("Y", &[])
        .rule("Z", &["Y"]);

    let grammar = builder.build();
    println!("Grammar:\n{}", grammar);

    let first_set = grammar.first_set();
    println!("First(EXPR): {:?}", first_set.get(&["EXPR"]));
    println!("First(FACTOR): {:?}", first_set.get(&["FACTOR"]));
}

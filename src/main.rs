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
    println!("First(X): {:?}", first_set.get(&["X"]));
    println!("First(Y): {:?}", first_set.get(&["Y"]));
    println!("First(Z): {:?}", first_set.get(&["Z"]));
    println!("First(Y,X): {:?}", first_set.get(&["Y", "X"]));
    println!("First(NUM): {:?}", first_set.get(&["NUM"]));
    println!("First(Y,NUM): {:?}", first_set.get(&["Y", "NUM"]));
    println!("First(Y,X): {:?}", first_set.get(&["Y", "X"]));
    println!("First(Y,X,NUM): {:?}", first_set.get(&["Y", "X", "NUM"]));
    println!("First(Y,Z,NUM): {:?}", first_set.get(&["Y", "Z", "NUM"]));
}

use lelele::{dfa::DFAGenerator, grammar::Grammar};

fn main() {
    let mut builder = Grammar::builder();
    builder
        .start("A")
        .terminals(["EQUAL", "PLUS", "ID", "NUM"])
        .rule("A", ["E", "EQUAL", "E"])
        .rule("A", ["ID"])
        .rule("E", ["E", "PLUS", "T"])
        .rule("E", ["T"])
        .rule("T", ["NUM"])
        .rule("T", ["ID"]);

    let grammar = builder.build();
    println!("Grammar:\n{}", grammar);

    // DFA construction
    let mut gen = DFAGenerator::new(&grammar);
    let nodes = gen.process();

    println!("\nDFA nodes:");
    for (id, node) in &nodes {
        println!(" - {:02}:", id);
        println!("     item_set:");
        for item in &node.item_set {
            let rule = &grammar.rules.get(&item.rule_id).unwrap();
            print!("       - [{} -> ", grammar.symbol_name(rule.lhs));
            for (i, s) in rule.rhs.iter().enumerate() {
                if i > 0 {
                    print!(" ");
                }
                if i == item.marker {
                    print!("@ ");
                }
                print!("{}", grammar.symbol_name(*s));
            }
            if item.marker == rule.rhs.len() {
                print!(" @");
            }
            println!("] {{ {} }}", grammar.symbol_name(item.lookahead));
        }
        if !node.edges.is_empty() {
            println!("     edges:");
            for (symbol, id) in &node.edges {
                println!("       - {} -> {:02}", grammar.symbol_name(*symbol), id);
            }
        }
    }
}

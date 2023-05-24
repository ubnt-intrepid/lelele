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
    let dfa = gen.process();

    println!("\nDFA:\n{}", dfa.display(&grammar));

    let transition_table = dfa.transition_table(&grammar);
    println!("\nTransition table:");
    for (id, actions) in &transition_table {
        println!(" - {:02}", id);
        for (input, action) in actions.iter() {
            println!("   - {}: {}", grammar.symbol_name(*input), action);
        }
    }
}

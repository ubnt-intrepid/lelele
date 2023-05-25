use lelele::{dfa::DFA, grammar::Grammar};

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
    let dfa = DFA::generate(&grammar);
    println!("\nDFA:\n{}", dfa);

    let transition_table = dfa.transition_table();
    println!("\nTransition table:");
    for (id, actions) in &transition_table {
        println!(" - {:02}", id);
        for (input, action) in actions.iter() {
            println!("   - {}: {}", grammar.symbol(*input).name(), action);
        }
    }
}

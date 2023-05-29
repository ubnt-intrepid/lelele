//! code generation.

use crate::{
    dfa::{Action, NodeID, DFA},
    grammar::{Grammar, RuleID, SymbolID},
};
use indexmap::{IndexMap, IndexSet};
use std::fmt;

#[derive(Debug)]
pub struct ParserDefinition<'g> {
    grammar: &'g Grammar<'g>,
    table: IndexMap<NodeID, IndexMap<SymbolID, Action>>,
    start_node: NodeID,

    // re-ordered identifiers and export names.
    node_ids: IndexSet<NodeID>,
    symbol_ids: IndexSet<SymbolID>,
    rule_ids: IndexMap<RuleID, Option<(SymbolID, usize)>>,
}

impl<'g> ParserDefinition<'g> {
    pub fn new(grammar: &'g Grammar<'g>) -> Self {
        let dfa = DFA::generate(&grammar);
        let start_node = dfa.start_node().0;
        let table = dfa.parse_table();

        // 使用されている NodeID, RuleID, SymbolID を集計し、コード生成用に並び換える
        let node_ids: IndexSet<NodeID> = table.keys().copied().collect();

        let mut symbol_ids: IndexSet<SymbolID> = IndexSet::new();
        symbol_ids.insert(SymbolID::EOI);
        symbol_ids.insert(SymbolID::START);
        symbol_ids.extend(grammar.symbols().map(|(id, _)| id));

        // 各 rule は `<LHS_NAME>_<i>` という名称で export される (iはgrammarへの登録順)
        // reordering と同時にその対応表も作成する
        let mut rule_ids: IndexMap<RuleID, Option<(SymbolID, usize)>> = IndexMap::new();
        let mut rule_names: IndexMap<SymbolID, IndexSet<RuleID>> = IndexMap::new();
        for (rule_id, rule) in grammar.rules() {
            match rule_id {
                RuleID::START => {
                    // identifier としては存在するが export しない
                    rule_ids.insert(RuleID::START, None);
                }
                rule_id => {
                    let lhs_rules = rule_names.entry(rule.lhs).or_default();
                    lhs_rules.insert(rule_id);
                    rule_ids.insert(
                        rule_id,
                        Some((rule.lhs, lhs_rules.get_index_of(&rule_id).unwrap())),
                    );
                }
            }
        }

        Self {
            grammar,
            start_node,
            table,
            node_ids,
            symbol_ids,
            rule_ids,
        }
    }

    fn node_id_of(&self, n: &NodeID) -> usize {
        self.node_ids.get_index_of(n).unwrap()
    }

    fn symbol_id_of(&self, s: &SymbolID) -> usize {
        self.symbol_ids.get_index_of(s).unwrap()
    }

    fn rule_id_and_name(&self, id: &RuleID) -> (usize, Option<(SymbolID, usize)>) {
        let (i, _, v) = self.rule_ids.get_full(id).unwrap();
        (i, *v)
    }

    fn fmt_preamble(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        const PREAMBLE: &str = "\
// This file is automatically generated by lelele.

use ::lelele_runtime::_private as lelele;

/// The alias to parser type using generated parser definition.
pub type Parser<TTok> = lelele::Parser<ParserDefinition, TTok>;

/// Create an instance of parser using generated definition.
pub fn parser<TTok>() -> Parser<TTok>
where
    TTok: lelele::Token<SymbolID>,
{
    lelele::Parser::new(
        ParserDefinition::default()
    )
}

/// The parser definition.
#[derive(Default)]
pub struct ParserDefinition {
    _p: (),
}

impl lelele::ParserDefinition for ParserDefinition {
    type State = NodeID;
    type Symbol = SymbolID;
    type Reduce = RuleID;

    #[inline]
    fn initial_state(&self) -> Self::State {
        NodeID::__START
    }

    #[inline]
    fn action(
        &self,
        current: Self::State,
        input: Option<Self::Symbol>,
    ) -> Result<lelele::ParserAction<Self::State, Self::Symbol, Self::Reduce>, lelele::ParserActionError> {
        let input = input.unwrap_or(SymbolID::__EOI).__raw;
        let actions = PARSE_TABLE.get(current.__raw).ok_or_else(|| lelele::ParserActionError::IncorrectState)?;
        actions.get(&input).copied().ok_or_else(|| lelele::ParserActionError::IncorrectSymbol)
    }
}
";
        f.write_str(PREAMBLE)
    }

    fn fmt_node_id_def(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(
            "\
/// The type to identify DFA state nodes.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct NodeID { __raw: usize }
impl NodeID {\n",
        )?;

        writeln!(
            f,
            "    const __START: Self = Self {{ __raw: {}_usize }};",
            self.node_id_of(&self.start_node)
        )?;

        f.write_str("}\n")?;

        Ok(())
    }

    fn fmt_symbol_id_def(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(
            "\
/// The type to identify terminal or nonterminal symbols used in generated DFA.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct SymbolID { __raw: u64 }
impl SymbolID {\n",
        )?;

        writeln!(
            f,
            "const __EOI: Self = Self {{ __raw: {}_u64 }};",
            self.symbol_id_of(&SymbolID::EOI),
        )?;

        for (id, symbol) in self
            .grammar
            .symbols()
            .filter(|(id, _)| *id != SymbolID::EOI && *id != SymbolID::START)
        {
            writeln!(
                f,
                "\
/// `{name}`
pub const {name}: Self = Self {{ __raw: {id}_u64 }};",
                name = symbol.name(),
                id = self.symbol_id_of(&id)
            )?;
        }

        f.write_str("}\n")?;

        Ok(())
    }

    fn fmt_rule_id_def(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(
            "\
/// The type to identify the syntax rule that matched input sequence.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct RuleID { __raw: u64 }
impl RuleID {\n",
        )?;

        for (rule_id, rule) in self.grammar.rules().filter(|(id, _)| *id != RuleID::START) {
            let (id, name) = self.rule_id_and_name(&rule_id);
            if let Some((sym, i)) = name {
                let comment_lhs = self.grammar.symbol(rule.lhs).name();
                let comment_rhs =
                    rule.rhs
                        .iter()
                        .enumerate()
                        .fold(String::new(), |mut acc, (i, s)| {
                            if i > 0 {
                                acc += " ";
                            }
                            acc += self.grammar.symbol(*s).name();
                            acc
                        });
                writeln!(f, "    /// `{} : {}`", comment_lhs, comment_rhs,)?;
                writeln!(
                    f,
                    "    pub const {symbol_name}_{i}: Self = Self {{ __raw: {id}_u64 }};",
                    symbol_name = self.grammar.symbol(sym).name(),
                    i = i,
                    id = id
                )?;
            }
        }

        f.write_str("}\n")?;

        Ok(())
    }

    fn fmt_parse_table(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(
            "\
const PARSE_TABLE: &[
    lelele::phf::Map<
        u64,
        lelele::ParserAction<
            NodeID,
            SymbolID,
            RuleID,
        >,
    >
] = &[\n",
        )?;

        for actions in self.table.values() {
            let mut actions_g = phf_codegen::Map::<u64>::new();
            actions_g.phf_path("lelele::phf");
            for (symbol, action) in actions {
                let action_g = match action {
                    Action::Shift(n) | Action::Goto(n) => {
                        format!(
                            "lelele::ParserAction::Shift(NodeID {{ __raw: {}_usize }})",
                            self.node_id_of(n)
                        )
                    }
                    Action::Reduce(r) => {
                        let rule = self.grammar.rule(*r);
                        format!(
                        "lelele::ParserAction::Reduce(RuleID {{ __raw: {}_u64 }}, SymbolID {{ __raw: {}_u64 }}, {}_usize)",
                        self.rule_id_and_name(r).0,
                        self.symbol_id_of(&rule.lhs),
                        rule.rhs.len()
                    )
                    }
                    Action::Accept => format!("lelele::ParserAction::Accept"),
                };
                actions_g.entry(self.symbol_id_of(&symbol) as u64, &action_g);
            }
            writeln!(f, "{},", actions_g.build())?;
        }

        f.write_str("];\n")?;

        Ok(())
    }
}

impl<'g> fmt::Display for ParserDefinition<'g> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_preamble(f)?;
        self.fmt_node_id_def(f)?;
        self.fmt_symbol_id_def(f)?;
        self.fmt_rule_id_def(f)?;
        self.fmt_parse_table(f)?;
        Ok(())
    }
}

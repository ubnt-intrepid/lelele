//! Grammar types.

use crate::{
    syntax as s,
    types::{Map, Set},
};
use std::{borrow::Cow, fs, hash::Hash, io, marker::PhantomData, path::Path};



/// The contextural values for building a `Grammar`.
#[derive(Debug)]
pub struct GrammarDef<'def> {
    terminals: Map<TerminalID, Terminal>,
    nonterminals: Map<NonterminalID, Nonterminal>,
    rules: Map<RuleID, Rule>,
    start: Option<NonterminalID>,
    next_terminal_id: u16,
    next_nonterminal_id: u16,
    next_rule_id: u16,
    _marker: PhantomData<&'def mut ()>,
}

impl<'def> GrammarDef<'def> {
    /// Declare a terminal symbol used in this grammar.
    pub fn terminal(
        &mut self,
        export_name: &str,
        precedence: Option<Precedence>,
    ) -> Result<TerminalID, GrammarDefError> {
        if !verify_ident(export_name) {
            return Err(GrammarDefError::Other {
                msg: "incorrect token name".into(),
            });
        }

        for terminal in self.terminals.values() {
            if matches!(terminal.export_name(), Some(name) if name == export_name) {
                return Err(GrammarDefError::Other {
                    msg: format!(
                        "The terminal `{}' has already been exported",
                        terminal.export_name().unwrap_or("<bogus>")
                    ),
                });
            }
        }

        let id = TerminalID::from_raw(self.next_terminal_id);
        self.next_terminal_id += 1;

        self.terminals.insert(
            id,
            Terminal {
                export_name: Some(export_name.to_owned().into()),
                precedence,
            },
        );

        Ok(id)
    }

    /// Declare a nonterminal symbol used in this grammar.
    pub fn nonterminal(&mut self, export_name: &str) -> Result<NonterminalID, GrammarDefError> {
        if !verify_ident(export_name) {
            return Err(GrammarDefError::Other {
                msg: "incorrect symbol name".into(),
            });
        }

        for nonterminal in self.nonterminals.values() {
            if matches!(nonterminal.export_name(), Some(name) if name == export_name) {
                return Err(GrammarDefError::Other {
                    msg: format!(
                        "The nonterminal export `{}' has already been used",
                        nonterminal.export_name().unwrap_or("<bogus>")
                    ),
                });
            }
        }

        let id = NonterminalID::new(self.next_nonterminal_id);
        self.next_nonterminal_id += 1;

        self.nonterminals.insert(
            id,
            Nonterminal {
                export_name: Some(export_name.to_owned().into()),
            },
        );

        Ok(id)
    }

    /// Specify a production rule into this grammer.
    pub fn rule<I>(
        &mut self,
        left: NonterminalID,
        right: I,
        precedence: Option<Precedence>,
    ) -> Result<(), GrammarDefError>
    where
        I: IntoIterator<Item = SymbolID>,
    {
        let right_ = right.into_iter().collect();
        for rule in self.rules.values() {
            if rule.left == left && rule.right == right_ {
                return Err(GrammarDefError::Other {
                    msg: "Duplicate production rule detected".into(),
                });
            }
        }

        let id = RuleID::new(self.next_rule_id);
        self.next_rule_id += 1;
        self.rules.insert(
            id,
            Rule {
                left,
                right: right_,
                precedence,
            },
        );

        Ok(())
    }

    /// Specify the start symbol for this grammar.
    pub fn start_symbol(&mut self, symbol: NonterminalID) -> Result<(), GrammarDefError> {
        self.start.replace(symbol);
        Ok(())
    }

    fn end(mut self) -> Result<Grammar, GrammarDefError> {
        // start symbolのID変換
        // 指定されていない場合は最初に登録されたnonterminal symbolを用いる
        let start = match self.start.take() {
            Some(start) => start,
            None => self
                .nonterminals
                .keys()
                .find(|id| **id != NonterminalID::START)
                .copied()
                .ok_or_else(|| GrammarDefError::Other {
                    msg: "empty nonterminal symbols".into(),
                })?,
        };

        self.rules.insert(
            RuleID::ACCEPT,
            Rule {
                left: NonterminalID::START,
                right: vec![SymbolID::N(start), SymbolID::T(TerminalID::EOI)],
                precedence: None,
            },
        );

        let mut nullables = Set::default();
        loop {
            let mut changed = false;
            for p in self.rules.values() {
                if p.right
                    .iter()
                    .all(|s| matches!(s, SymbolID::N(n) if nullables.contains(n)))
                {
                    changed |= nullables.insert(p.left);
                }
            }
            if !changed {
                break;
            }
        }

        Ok(Grammar {
            terminals: self.terminals,
            nonterminals: self.nonterminals,
            rules: self.rules,
            start_symbol: start,
            nullables,
        })
    }
}


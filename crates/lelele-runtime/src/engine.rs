//! The implementation of LR(1) parser engine.

use crate::definition::{ParseActionKind::*, ParserDef, Terminal};
use std::fmt;

/// A trait for abstracting token symbols.
pub trait Token<TIdx> {
    /// Return the index value corresponding to this token.
    fn to_index(&self) -> TIdx;
}

/// The instance of LR(1) parser engine that drives incrementally, based on generated parser definition.
pub struct ParseEngine<TDef, TTok>
where
    TDef: ParserDef,
    TTok: Token<TDef::TerminalIndex>,
{
    definition: TDef,
    state: ParseEngineState<TDef>,
    states_stack: Vec<TDef::StateIndex>,
    symbols_stack: Vec<Symbol<TDef, TTok>>,
    lookahead: Option<Option<TTok>>,
    num_recovered: usize,
}

impl<TDef, TTok> fmt::Debug for ParseEngine<TDef, TTok>
where
    TDef: ParserDef + fmt::Debug,
    TDef::StateIndex: fmt::Debug,
    TDef::TerminalIndex: fmt::Debug,
    TDef::NonterminalIndex: fmt::Debug,
    TTok: Token<TDef::TerminalIndex> + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Parser")
            .field("definition", &self.definition)
            .field("state", &self.state)
            .field("state_stack", &self.states_stack)
            .field("item_stack", &self.symbols_stack)
            .field("lookahead", &self.lookahead)
            .field("num_recovered", &self.num_recovered)
            .finish_non_exhaustive()
    }
}

enum ParseEngineState<TDef>
where
    TDef: ParserDef,
{
    Pending,
    WaitingInput,
    Shifting(TDef::StateIndex, bool),
    Reducing(TDef::NonterminalIndex, usize),
    HandlingError,
    Accepted,
    Rejected,
}

impl<TDef> fmt::Debug for ParseEngineState<TDef>
where
    TDef: ParserDef,
    TDef::StateIndex: fmt::Debug,
    TDef::TerminalIndex: fmt::Debug,
    TDef::NonterminalIndex: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Pending => f.debug_struct("Pending").finish(),
            Self::WaitingInput => f.debug_struct("WaitingInput").finish(),
            Self::Shifting(next, recovered) => f
                .debug_tuple("Shifting")
                .field(next)
                .field(recovered)
                .finish(),
            Self::Reducing(symbol, n) => f.debug_tuple("Reducing").field(symbol).field(n).finish(),
            Self::HandlingError => f.debug_struct("HandlingError").finish(),
            Self::Accepted => f.debug_struct("Accepted").finish(),
            Self::Rejected => f.debug_struct("Rejected").finish(),
        }
    }
}

impl<TDef, TTok> ParseEngine<TDef, TTok>
where
    TDef: ParserDef,
    TTok: Token<TDef::TerminalIndex>,
{
    /// Create a parser engine using the specified parser definition.
    pub fn new(definition: TDef) -> Self {
        let initial_state = definition.initial_state();
        Self {
            definition,
            states_stack: vec![initial_state],
            symbols_stack: vec![],
            state: ParseEngineState::Pending,
            lookahead: None,
            num_recovered: 0,
        }
    }

    pub fn offer_token(&mut self, token: TTok) -> Result<Option<TTok>, ParseError> {
        if !matches!(self.state, ParseEngineState::WaitingInput) {
            return Err(ParseError::AlreadyOfferredInput);
        }
        self.state = ParseEngineState::Pending;
        match self.lookahead {
            Some(..) => Ok(Some(token)),
            None => {
                self.lookahead.replace(Some(token));
                Ok(None)
            }
        }
    }

    pub fn offer_eoi(&mut self) -> Result<bool, ParseError> {
        if !matches!(self.state, ParseEngineState::WaitingInput) {
            return Err(ParseError::AlreadyOfferredInput);
        }
        self.state = ParseEngineState::Pending;
        match self.lookahead {
            Some(..) => Ok(false),
            None => {
                self.lookahead.replace(None);
                Ok(true)
            }
        }
    }

    /// Drives the internal LR automaton to the point where the user of this parser needs to do something.
    pub fn resume(&mut self) -> Result<ParseEvent<'_, TDef, TTok>, ParseError> {
        match self.state {
            ParseEngineState::WaitingInput => {
                return Err(ParseError::TokenNotOffered);
            }

            ParseEngineState::Shifting(next, recovered) => {
                let lookahead = if recovered {
                    Symbol::ErrorToken
                } else {
                    let tok = self
                        .lookahead
                        .take()
                        .ok_or_else(|| ParseError::TokenNotOffered)?;
                    match tok {
                        Some(tok) => Symbol::T(tok),
                        None => panic!("shifting token must not be EOI"),
                    }
                };
                self.symbols_stack.push(lookahead);
                self.states_stack.push(next);
                self.state = ParseEngineState::Pending;
            }

            ParseEngineState::Reducing(lhs, n) => {
                self.states_stack.truncate(self.states_stack.len() - n);
                self.symbols_stack.truncate(self.symbols_stack.len() - n);

                let current = self.states_stack.last().copied().unwrap();
                self.symbols_stack.push(Symbol::N(lhs));
                let next = self.definition.goto(current, lhs);
                self.states_stack.push(next);
                self.state = ParseEngineState::Pending;
            }

            ParseEngineState::HandlingError => {
                let current = self.states_stack.last().copied().unwrap();
                let action = self.definition.action(current, Terminal::Error);
                match action {
                    Shift(next) => {
                        self.num_recovered += 1;
                        self.state = ParseEngineState::Shifting(next, true);
                        return Ok(ParseEvent::Shifting(Terminal::Error));
                    }
                    Reduce(..) | Accept => unreachable!(),
                    Fail => {
                        self.state = ParseEngineState::Rejected;
                        return Ok(ParseEvent::Rejected);
                    }
                }
            }

            ParseEngineState::Accepted | ParseEngineState::Rejected => {
                return Err(ParseError::AlreadyAccepted);
            }

            ParseEngineState::Pending => {}
        }

        let current = self.states_stack.last().copied().unwrap();
        let lookahead = match self.lookahead {
            Some(ref lookahead) => lookahead.as_ref(),
            None => {
                self.state = ParseEngineState::WaitingInput;
                return Ok(ParseEvent::InputNeeded);
            }
        };

        let action = self.definition.action(
            current,
            match lookahead {
                Some(t) => Terminal::T(t.to_index()),
                None => Terminal::EOI,
            },
        );

        match action {
            Shift(next) => match lookahead {
                Some(lookahead) => {
                    self.state = ParseEngineState::Shifting(next, false);
                    return Ok(ParseEvent::Shifting(Terminal::T(lookahead)));
                }
                None => unreachable!(),
            },

            Reduce(lhs, n) => {
                self.state = ParseEngineState::Reducing(lhs, n);
                let args = &self.symbols_stack[self.symbols_stack.len() - n..];
                return Ok(ParseEvent::AboutToReduce(lhs, args));
            }

            Accept => {
                self.state = ParseEngineState::Accepted;
                return Ok(ParseEvent::Accepted(self.num_recovered));
            }

            Fail => {
                self.state = ParseEngineState::HandlingError;
                return Ok(ParseEvent::HandlingError {
                    state: current,
                    lookahead,
                    expected: self.definition.expected_terminals(current),
                });
            }
        }
    }
}

#[derive(Debug)]
pub enum ParseEvent<'p, TDef, TTok>
where
    TDef: ParserDef,
    TTok: Token<TDef::TerminalIndex>,
{
    InputNeeded,
    Shifting(Terminal<&'p TTok>),
    AboutToReduce(TDef::NonterminalIndex, &'p [Symbol<TDef, TTok>]),
    HandlingError {
        state: TDef::StateIndex,
        lookahead: Option<&'p TTok>,
        expected: &'p [Terminal<TDef::TerminalIndex>],
    },
    Accepted(usize),
    Rejected,
}

#[derive(Debug, Copy, Clone, PartialEq)]
#[non_exhaustive]
pub enum Symbol<TDef, TTok>
where
    TDef: ParserDef,
    TTok: Token<TDef::TerminalIndex>,
{
    T(TTok),
    N(TDef::NonterminalIndex),
    ErrorToken,

    #[doc(hidden)]
    __Empty,
}

impl<TDef, TTok> Default for Symbol<TDef, TTok>
where
    TDef: ParserDef,
    TTok: Token<TDef::TerminalIndex>,
{
    fn default() -> Self {
        Self::__Empty
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error("from parser definition: {}", _0)]
    ParserDef(String),

    #[error("token is not offered")]
    TokenNotOffered,

    #[error("token has already been offerred")]
    AlreadyOfferredInput,

    #[error("unexpected EOI")]
    UnexpectedEOI,

    #[error("already accepted")]
    AlreadyAccepted,
}

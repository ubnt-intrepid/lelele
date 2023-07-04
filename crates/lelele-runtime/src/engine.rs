//! The implementation of LR(1) parser engine.

use crate::definition::{ParserDef, Terminal};
use std::{fmt, marker::PhantomData};

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
            .finish_non_exhaustive()
    }
}

enum ParseEngineState<TDef>
where
    TDef: ParserDef,
{
    Pending,
    WaitingInput,
    Shifting(TDef::StateIndex),
    Reducing(TDef::NonterminalIndex, usize),
    HandlingError,
    Accepted,
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
            Self::Shifting(next) => f.debug_tuple("Shifting").field(next).finish(),
            Self::Reducing(symbol, n) => f.debug_tuple("Reducing").field(symbol).field(n).finish(),
            Self::HandlingError => f.debug_struct("HandlingError").finish(),
            Self::Accepted => f.debug_struct("Accepted").finish(),
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

            ParseEngineState::Shifting(next) => {
                let lookahead = self
                    .lookahead
                    .take()
                    .ok_or_else(|| ParseError::TokenNotOffered)?;
                let lookahead = lookahead.expect("shiting token must not be EOI");
                self.symbols_stack.push(Symbol::T(lookahead));
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
                return Ok(ParseEvent::Rejected);
            }

            ParseEngineState::Accepted => {
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

        let action = self
            .definition
            .action(
                current,
                lookahead.map(|t| t.to_index()),
                ParseContext {
                    _marker: PhantomData,
                },
            )
            .unwrap();

        match action {
            ParseAction::Shift(next) => match lookahead {
                Some(lookahead) => {
                    self.state = ParseEngineState::Shifting(next);
                    return Ok(ParseEvent::Shifting(lookahead));
                }
                None => unreachable!(),
            },

            ParseAction::Reduce(lhs, n) => {
                self.state = ParseEngineState::Reducing(lhs, n);
                let args = &self.symbols_stack[self.symbols_stack.len() - n..];
                return Ok(ParseEvent::AboutToReduce(lhs, args));
            }

            ParseAction::Accept => {
                self.state = ParseEngineState::Accepted;
                return Ok(ParseEvent::Accepted);
            }

            ParseAction::Fail => {
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
    Shifting(&'p TTok),
    AboutToReduce(TDef::NonterminalIndex, &'p [Symbol<TDef, TTok>]),
    HandlingError {
        state: TDef::StateIndex,
        lookahead: Option<&'p TTok>,
        expected: &'p [Terminal<TDef::TerminalIndex>],
    },
    Accepted,
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

// ---- ParseAction ----

enum ParseAction<TState, TSymbol> {
    Shift(TState),
    Reduce(TSymbol, usize),
    Accept,
    Fail,
}

struct ParseContext<TState, TToken, TSymbol> {
    _marker: PhantomData<(TState, TToken, TSymbol)>,
}

impl<TState: Copy, TToken: Copy, TSymbol: Copy> crate::definition::ParseAction
    for ParseContext<TState, TToken, TSymbol>
{
    type StateIndex = TState;
    type TerminalIndex = TToken;
    type NonterminalIndex = TSymbol;

    type Ok = ParseAction<TState, TSymbol>;
    type Error = std::convert::Infallible;

    fn shift(self, next: Self::StateIndex) -> Result<Self::Ok, Self::Error> {
        Ok(ParseAction::Shift(next))
    }

    fn reduce(self, s: Self::NonterminalIndex, n: usize) -> Result<Self::Ok, Self::Error> {
        Ok(ParseAction::Reduce(s, n))
    }

    fn accept(self) -> Result<Self::Ok, Self::Error> {
        Ok(ParseAction::Accept)
    }

    fn fail(self) -> Result<Self::Ok, Self::Error> {
        Ok(ParseAction::Fail)
    }
}

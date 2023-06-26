//! Parser.

use crate::definition::ParserDef;
use std::{fmt, marker::PhantomData};

/// A trait for abstracting token symbols.
pub trait Token<TTok> {
    fn as_symbol(&self) -> TTok;
}

impl<TTok: Copy> Token<TTok> for TTok {
    fn as_symbol(&self) -> TTok {
        *self
    }
}

/// The parser driven based on the generated parse table.
pub struct Parser<TDef, TTok>
where
    TDef: ParserDef,
    TTok: Token<TDef::Token>,
{
    definition: TDef,
    state: ParserState<TDef>,
    state_stack: Vec<TDef::State>,
    item_stack: Vec<ParseItem<TTok, TDef::Symbol>>,
    lookahead: Option<Option<TTok>>,
}
impl<TDef, TTok> fmt::Debug for Parser<TDef, TTok>
where
    TDef: ParserDef + fmt::Debug,
    TDef::State: fmt::Debug,
    TDef::Token: fmt::Debug,
    TDef::Symbol: fmt::Debug,
    TDef::Reduce: fmt::Debug,
    TTok: Token<TDef::Token> + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Parser")
            .field("definition", &self.definition)
            .field("state", &self.state)
            .field("state_stack", &self.state_stack)
            .field("item_stack", &self.item_stack)
            .field("lookahead", &self.lookahead)
            .finish_non_exhaustive()
    }
}

enum ParserState<TDef>
where
    TDef: ParserDef,
{
    Pending,
    Shifting(TDef::State),
    Reducing(TDef::Symbol, usize),
    Accepting,
    HandlingError,
    Accepted,
}
impl<TDef> fmt::Debug for ParserState<TDef>
where
    TDef: ParserDef,
    TDef::State: fmt::Debug,
    TDef::Token: fmt::Debug,
    TDef::Symbol: fmt::Debug,
    TDef::Reduce: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Pending => f.debug_struct("Pending").finish(),
            Self::Shifting(next) => f.debug_tuple("Shifting").field(next).finish(),
            Self::Reducing(symbol, n) => f.debug_tuple("Reducing").field(symbol).field(n).finish(),
            Self::Accepting => f.debug_struct("Accepting").finish(),
            Self::HandlingError => f.debug_struct("HandlingError").finish(),
            Self::Accepted => f.debug_struct("Accepted").finish(),
        }
    }
}

impl<TDef, TTok> Parser<TDef, TTok>
where
    TDef: ParserDef,
    TTok: Token<TDef::Token>,
{
    /// Create an instance of `Parser` using the specified parse table.
    pub fn new(definition: TDef) -> Self {
        let initial_state = definition.initial_state();
        Self {
            definition,
            state_stack: vec![initial_state],
            item_stack: vec![],
            state: ParserState::Pending,
            lookahead: None,
        }
    }

    /// Consume some tokens and drive the state machine until it matches a certain production rule.
    pub fn resume(&mut self) -> Result<ParseEvent<'_, TDef, TTok>, ParseError> {
        match self.state {
            ParserState::Shifting(next) => {
                let lookahead = self.lookahead.take().unwrap();
                self.item_stack.push(ParseItem::T(lookahead));
                self.state_stack.push(next);
                self.state = ParserState::Pending;
            }

            ParserState::Reducing(lhs, n) => {
                self.state_stack.truncate(self.state_stack.len() - n);
                self.item_stack.truncate(self.item_stack.len() - n);

                let current = self.state_stack.last().copied().unwrap();
                let next = self.definition.goto(current, lhs);
                self.state_stack.push(next);
                self.item_stack.push(ParseItem::N(lhs));
                self.state = ParserState::Pending;
            }

            ParserState::Accepting => {
                self.item_stack.pop().unwrap();
                self.state = ParserState::Accepted;
                return Ok(ParseEvent::Accepted);
            }

            ParserState::HandlingError => {
                return Ok(ParseEvent::Rejected);
            }

            ParserState::Accepted => {
                return Err(ParseError::AlreadyAccepted);
            }

            ParserState::Pending => {}
        }

        let current = self.state_stack.last().copied().unwrap();
        let lookahead = match self.lookahead {
            Some(ref lookahead) => lookahead.as_ref().map(|t| t.as_symbol()),
            None => {
                return Ok(ParseEvent::InputNeeded(SinkInput {
                    lookahead: &mut self.lookahead,
                }))
            }
        };
        let action = self
            .definition
            .action(
                current,
                lookahead,
                ParseContext {
                    _marker: PhantomData,
                },
            )
            .unwrap();

        match action {
            ParseAction::Shift(next) => {
                let lookahead = self.lookahead.as_ref().unwrap();
                self.state = ParserState::Shifting(next);
                return Ok(ParseEvent::Shifting(lookahead.as_ref()));
            }

            ParseAction::Reduce(reduce, lhs, n) => {
                self.state = ParserState::Reducing(lhs, n);
                let args = &self.item_stack[self.item_stack.len() - n..];
                return Ok(ParseEvent::AboutToReduce(reduce, args));
            }

            ParseAction::Accept => {
                self.state = ParserState::Accepting;
                return Ok(ParseEvent::AboutToAccept(self.item_stack.last().unwrap()));
            }

            ParseAction::Fail { expected } => {
                self.state = ParserState::HandlingError;
                let lr_state = self.state_stack.last().unwrap();
                let lookahead = self.lookahead.as_ref().unwrap().as_ref();
                return Ok(ParseEvent::HandlingError {
                    lr_state,
                    lookahead,
                    expected,
                });
            }
        }
    }
}

#[derive(Debug)]
pub enum ParseEvent<'p, TDef, TTok>
where
    TDef: ParserDef,
    TTok: Token<TDef::Token>,
{
    InputNeeded(SinkInput<'p, TTok>),
    Shifting(Option<&'p TTok>),
    AboutToReduce(TDef::Reduce, &'p [ParseItem<TTok, TDef::Symbol>]),
    AboutToAccept(&'p ParseItem<TTok, TDef::Symbol>),
    HandlingError {
        lr_state: &'p TDef::State,
        lookahead: Option<&'p TTok>,
        expected: Vec<TDef::Token>,
    },
    Accepted,
    Rejected,
}

#[derive(Debug)]
pub struct SinkInput<'p, TTok> {
    lookahead: &'p mut Option<Option<TTok>>,
}
impl<'p, TTok> SinkInput<'p, TTok> {
    pub fn offer_token(self, token: TTok) -> Option<TTok> {
        match self.lookahead {
            Some(..) => Some(token),
            None => {
                self.lookahead.replace(Some(token));
                None
            }
        }
    }

    pub fn offer_eoi(self) -> bool {
        match self.lookahead {
            Some(..) => false,
            None => {
                self.lookahead.replace(None);
                true
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
#[non_exhaustive]
pub enum ParseItem<TTok, TSym> {
    T(Option<TTok>),
    N(TSym),

    #[doc(hidden)]
    __Empty,
}

impl<TTok, TSym> Default for ParseItem<TTok, TSym> {
    fn default() -> Self {
        Self::__Empty
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error("from parser definition: {}", _0)]
    ParserDef(String),

    #[error("unexpected EOI")]
    UnexpectedEOI,

    #[error("already accepted")]
    AlreadyAccepted,
}

// ---- ParseAction ----

enum ParseAction<TState, TToken, TSymbol, TReduce> {
    Shift(TState),
    Reduce(TReduce, TSymbol, usize),
    Accept,
    Fail { expected: Vec<TToken> },
}

struct ParseContext<TState, TToken, TSymbol, TReduce> {
    _marker: PhantomData<(TState, TToken, TSymbol, TReduce)>,
}

impl<TState: Copy, TToken: Copy, TSymbol: Copy, TReduce> crate::definition::ParseAction
    for ParseContext<TState, TToken, TSymbol, TReduce>
{
    type State = TState;
    type Token = TToken;
    type Symbol = TSymbol;
    type Reduce = TReduce;

    type Ok = ParseAction<TState, TToken, TSymbol, TReduce>;
    type Error = std::convert::Infallible;

    fn shift(self, next: Self::State) -> Result<Self::Ok, Self::Error> {
        Ok(ParseAction::Shift(next))
    }

    fn reduce(self, r: Self::Reduce, s: Self::Symbol, n: usize) -> Result<Self::Ok, Self::Error> {
        Ok(ParseAction::Reduce(r, s, n))
    }

    fn accept(self) -> Result<Self::Ok, Self::Error> {
        Ok(ParseAction::Accept)
    }

    fn fail<I>(self, expected_tokens: I) -> Result<Self::Ok, Self::Error>
    where
        I: IntoIterator<Item = Self::Token>,
    {
        Ok(ParseAction::Fail {
            expected: expected_tokens.into_iter().collect(),
        })
    }
}

//! Parser.

use crate::definition::ParserDef;
use std::marker::PhantomData;

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
#[derive(Debug)]
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

#[derive(Debug)]
enum ParserState<TDef>
where
    TDef: ParserDef,
{
    Pending,
    Shifting(TDef::State),
    Reducing(TDef::Symbol, usize),
    Accepting,
    Accepted,
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

    pub fn offer_token(&mut self, token: TTok) -> Option<TTok> {
        match self.lookahead {
            Some(..) => Some(token),
            None => {
                self.lookahead.replace(Some(token));
                None
            }
        }
    }

    pub fn offer_eoi(&mut self) -> bool {
        match self.lookahead {
            Some(..) => false,
            None => {
                self.lookahead.replace(None);
                true
            }
        }
    }

    /// Consume some tokens and drive the state machine until it matches a certain production rule.
    pub fn next_event(&mut self) -> Result<ParseEvent<'_, TDef, TTok>, ParseError<TDef::Token>> {
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

            ParserState::Accepted => {
                return Err(ParseError::AlreadyAccepted);
            }

            ParserState::Pending => {}
        }

        let current = self.state_stack.last().copied().unwrap();
        let lookahead = match self.lookahead {
            Some(ref lookahead) => lookahead.as_ref().map(|t| t.as_symbol()),
            None => return Ok(ParseEvent::InputNeeded),
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
                // FIXME: handle parse table errors
                return Err(ParseError::Syntax {
                    expected,
                    lookahead,
                });
            }
        }
    }
}

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

#[derive(Debug)]
pub enum ParseEvent<'p, TDef, TTok>
where
    TDef: ParserDef,
    TTok: Token<TDef::Token>,
{
    InputNeeded,
    Shifting(Option<&'p TTok>),
    AboutToReduce(TDef::Reduce, &'p [ParseItem<TTok, TDef::Symbol>]),
    AboutToAccept(&'p ParseItem<TTok, TDef::Symbol>),
    Accepted,
}

#[derive(Debug, thiserror::Error)]
pub enum ParseError<TToken> {
    #[error("from parser definition: {}", _0)]
    ParserDef(String),

    #[error("syntax error")]
    Syntax {
        expected: Vec<TToken>,
        lookahead: Option<TToken>,
    },

    #[error("unexpected EOI")]
    UnexpectedEOI,

    #[error("already accepted")]
    AlreadyAccepted,
}

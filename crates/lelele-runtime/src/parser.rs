//! Parser.

use crate::definition::ParserDef;
use std::{marker::PhantomData, mem};

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
    state_stack: Vec<TDef::State>,
    item_stack: Vec<StackItem<TTok, TDef::Symbol>>,
    parser_state: ParserState<TDef::Symbol>,
    lookahead: Option<Option<TTok>>,
}

#[derive(Debug)]
enum ParserState<TSymbol> {
    Reading,
    PendingGoto(TSymbol),
    Accepted,
}

#[derive(Debug)]
enum StackItem<TTok, TSymbol> {
    T(Option<TTok>),
    N(TSymbol),
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
            parser_state: ParserState::Reading,
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

    /// Consume some tokens and drive the state machine
    /// until it matches a certain production rule.
    pub fn next_event(
        &mut self,
        args: &mut Vec<ParseItem<TTok, TDef::Symbol>>,
    ) -> Result<ParseEvent<TDef>, ParseError<TDef::Token>> {
        match self.parser_state {
            ParserState::PendingGoto(symbol) => {
                let current = self.state_stack.last().copied().unwrap();
                let next = self.definition.goto(current, symbol);
                self.state_stack.push(next);
                self.item_stack.push(StackItem::N(symbol));
                self.parser_state = ParserState::Reading;
            }
            ParserState::Accepted => {
                return Err(ParseError::AlreadyAccepted);
            }
            _ => (),
        }

        loop {
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
                    let lookahead = self.lookahead.take().unwrap();
                    self.item_stack.push(StackItem::T(lookahead));
                    self.state_stack.push(next);
                    self.parser_state = ParserState::Reading;
                    continue;
                }

                ParseAction::Reduce(reduce, lhs, n) => {
                    args.resize_with(n, Default::default);
                    for i in 0..n {
                        self.state_stack.pop();
                        let item = self.item_stack.pop().unwrap();
                        args[n - i - 1] = match item {
                            StackItem::N(symbol) => ParseItem::N(symbol),
                            StackItem::T(Some(tok)) => ParseItem::T(tok),
                            StackItem::T(None) => unreachable!(),
                        };
                    }
                    self.parser_state = ParserState::PendingGoto(lhs);

                    return Ok(ParseEvent::AboutToReduce(reduce));
                }

                ParseAction::Accept => {
                    args.clear();
                    let item = self.item_stack.pop().unwrap();
                    args.push(match item {
                        StackItem::N(symbol) => ParseItem::N(symbol),
                        StackItem::T(..) => unreachable!(),
                    });

                    self.parser_state = ParserState::Accepted;
                    return Ok(ParseEvent::Accepted);
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

#[derive(Debug)]
#[non_exhaustive]
pub enum ParseItem<TTok, TSym> {
    T(TTok),
    N(TSym),

    #[doc(hidden)]
    __Empty,
}

impl<TTok, TSym> Default for ParseItem<TTok, TSym> {
    fn default() -> Self {
        Self::__Empty
    }
}

impl<TTok, TSym> ParseItem<TTok, TSym> {
    pub fn take(&mut self) -> Option<Self> {
        match mem::replace(self, Self::__Empty) {
            Self::__Empty => None,
            me => Some(me),
        }
    }
}

#[derive(Debug)]
pub enum ParseEvent<TDef>
where
    TDef: ParserDef,
{
    InputNeeded,
    AboutToReduce(TDef::Reduce),
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

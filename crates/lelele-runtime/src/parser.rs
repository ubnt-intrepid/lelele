//! Parser.

use crate::definition::ParserDef;
use std::{marker::PhantomData, mem};

/// A trait for abstracting token symbols.
pub trait Token<TTok> {
    fn as_symbol(&self) -> TTok;
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
    item_stack: Vec<ParseItem<TTok, TDef::Symbol>>,
    parser_state: ParserState<TDef::Symbol>,
    peeked_token: Option<TTok>,
}

#[derive(Debug)]
enum ParserState<TSymbol> {
    Reading,
    PendingGoto(TSymbol),
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
            parser_state: ParserState::Reading,
            peeked_token: None,
        }
    }

    /// Consume some tokens and drive the state machine
    /// until it matches a certain production rule.
    pub fn next_event<I, E>(
        &mut self,
        tokens: &mut I,
        args: &mut Vec<ParseItem<TTok, TDef::Symbol>>,
    ) -> Result<ParseEvent<TDef>, ParseError<E, TDef::Token>>
    where
        I: Iterator<Item = Result<TTok, E>>,
    {
        match self.parser_state {
            ParserState::PendingGoto(s) => {
                let current = self
                    .state_stack
                    .last()
                    .copied()
                    .ok_or_else(|| ParseError::EmptyNodeStack)?;
                let next = self.definition.goto(current, s);
                self.parser_state = ParserState::Reading;
                self.state_stack.push(next);
            }
            ParserState::Accepted => return Err(ParseError::AlreadyAccepted),
            _ => (),
        }

        loop {
            let current = self
                .state_stack
                .last()
                .copied()
                .ok_or_else(|| ParseError::EmptyNodeStack)?;

            let lookahead = {
                if self.peeked_token.is_none() {
                    self.peeked_token = tokens.next().transpose().map_err(ParseError::Lexer)?;
                }
                self.peeked_token.as_ref().map(|t| t.as_symbol())
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
                .map_err(|_| ParseError::ParserDef("error".into()))?;

            match action {
                ParseAction::Shift(n) => {
                    let t = match self.peeked_token.take() {
                        Some(t) => t,
                        None => tokens
                            .next()
                            .transpose()
                            .map_err(ParseError::Lexer)?
                            .ok_or_else(|| ParseError::UnexpectedEOI)?,
                    };
                    self.item_stack.push(ParseItem::T(t));
                    self.state_stack.push(n);
                    self.parser_state = ParserState::Reading;
                    continue;
                }

                ParseAction::Reduce(reduce, lhs, n) => {
                    args.resize_with(n, Default::default);
                    for i in 0..n {
                        self.state_stack.pop();
                        let arg = self
                            .item_stack
                            .pop()
                            .ok_or_else(|| ParseError::EmptyItemStack)?;
                        args[n - i - 1] = arg;
                    }
                    self.parser_state = ParserState::PendingGoto(lhs);
                    return Ok(ParseEvent::Reduce(reduce));
                }

                ParseAction::Accept => {
                    let arg = self
                        .item_stack
                        .pop()
                        .ok_or_else(|| ParseError::EmptyItemStack)?;
                    args.clear();
                    args.push(arg);

                    self.parser_state = ParserState::Accepted;
                    return Ok(ParseEvent::Accept);
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
    type Error = ();

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
    Reduce(TDef::Reduce),
    Accept,
}

#[derive(Debug, thiserror::Error)]
pub enum ParseError<L, TToken> {
    #[error("from lexer")]
    Lexer(L),

    #[error("from parser definition: {}", _0)]
    ParserDef(String),

    #[error("syntax error")]
    Syntax {
        expected: Vec<TToken>,
        lookahead: Option<TToken>,
    },

    #[error("unexpected EOI")]
    UnexpectedEOI,

    #[error("empty node stack")]
    EmptyNodeStack,

    #[error("empty item stack")]
    EmptyItemStack,

    #[error("already accepted")]
    AlreadyAccepted,
}

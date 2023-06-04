//! Parser.

use crate::definition::{ParseAction, ParseActionError, ParseTable};
use std::{fmt, mem};

/// A trait for abstracting token symbols.
pub trait Token<TSym> {
    fn as_symbol(&self) -> TSym;
}

/// The parser driven based on the generated parse table.
#[derive(Debug)]
pub struct Parser<TDef, TTok>
where
    TDef: ParseTable,
    TTok: Token<TDef::Symbol>,
{
    definition: TDef,
    state_stack: Vec<TDef::State>,
    item_stack: Vec<ParseItem<TTok, TDef::Symbol>>,
    parser_state: ParserState,
    peeked_token: Option<TTok>,
}

#[derive(Debug)]
enum ParserState {
    Reading,
    PendingGoto,
    Accepted,
}

impl<TDef, TTok> Parser<TDef, TTok>
where
    TDef: ParseTable,
    TTok: Token<TDef::Symbol>,
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
    ) -> Result<ParseEvent<TDef>, ParseError<E>>
    where
        I: Iterator<Item = Result<TTok, E>>,
        E: fmt::Display,
    {
        loop {
            let current = self
                .state_stack
                .last()
                .ok_or_else(|| ParseError::EmptyNodeStack)?;

            let input = match self.parser_state {
                ParserState::PendingGoto => match self
                    .item_stack
                    .last()
                    .ok_or_else(|| ParseError::EmptyItemStack)?
                {
                    ParseItem::N(s) => Some(*s),
                    ParseItem::T(t) => Some(t.as_symbol()),
                    _ => unreachable!(),
                },
                _ => {
                    if self.peeked_token.is_none() {
                        self.peeked_token = tokens.next().transpose().map_err(ParseError::Lexer)?;
                    }
                    self.peeked_token.as_ref().map(|t| t.as_symbol())
                }
            };

            match self.definition.action(*current, input) {
                ParseAction::Shift(n) => {
                    if !matches!(self.parser_state, ParserState::PendingGoto) {
                        let t = match self.peeked_token.take() {
                            Some(t) => t,
                            None => tokens
                                .next()
                                .transpose()
                                .map_err(ParseError::Lexer)?
                                .ok_or_else(|| ParseError::UnexpectedEOI)?,
                        };
                        self.item_stack.push(ParseItem::T(t));
                    }

                    self.parser_state = ParserState::Reading;
                    self.state_stack.push(n);
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

                    self.item_stack.push(ParseItem::N(lhs));
                    self.parser_state = ParserState::PendingGoto;

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

                ParseAction::Error(err) => {
                    // FIXME: handle parse table errors
                    return Err(ParseError::ParserDef(err));
                }
            }
        }
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
    TDef: ParseTable,
{
    Reduce(TDef::Reduce),
    Accept,
}

#[derive(Debug, thiserror::Error)]
pub enum ParseError<L: fmt::Display> {
    #[error("from lexer: {}", _0)]
    Lexer(L),

    #[error("from parser definition: {}", _0)]
    ParserDef(ParseActionError),

    #[error("unexpected EOI")]
    UnexpectedEOI,

    #[error("empty node stack")]
    EmptyNodeStack,

    #[error("empty item stack")]
    EmptyItemStack,
}

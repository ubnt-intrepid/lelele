//! Parser.

use std::error::Error;

pub trait ParserDefinition {
    type State: Copy;
    type Symbol: Copy;
    type Reduce;
    type Error: Error + Send + Sync + 'static;
    fn initial_state(&self) -> Self::State;
    fn action(
        &self,
        current: Self::State,
        input: Option<Self::Symbol>,
    ) -> Result<ParserAction<Self::State, Self::Symbol, Self::Reduce>, Self::Error>;
}

impl<T: ?Sized> ParserDefinition for &T
where
    T: ParserDefinition,
{
    type State = T::State;
    type Symbol = T::Symbol;
    type Reduce = T::Reduce;
    type Error = T::Error;

    fn initial_state(&self) -> Self::State {
        (**self).initial_state()
    }

    fn action(
        &self,
        current: Self::State,
        input: Option<Self::Symbol>,
    ) -> Result<ParserAction<Self::State, Self::Symbol, Self::Reduce>, Self::Error> {
        (**self).action(current, input)
    }
}

impl<T: ?Sized> ParserDefinition for std::rc::Rc<T>
where
    T: ParserDefinition,
{
    type State = T::State;
    type Symbol = T::Symbol;
    type Reduce = T::Reduce;
    type Error = T::Error;

    fn initial_state(&self) -> Self::State {
        (**self).initial_state()
    }

    fn action(
        &self,
        current: Self::State,
        input: Option<Self::Symbol>,
    ) -> Result<ParserAction<Self::State, Self::Symbol, Self::Reduce>, Self::Error> {
        (**self).action(current, input)
    }
}

impl<T: ?Sized> ParserDefinition for std::sync::Arc<T>
where
    T: ParserDefinition,
{
    type State = T::State;
    type Symbol = T::Symbol;
    type Reduce = T::Reduce;
    type Error = T::Error;

    fn initial_state(&self) -> Self::State {
        (**self).initial_state()
    }

    fn action(
        &self,
        current: Self::State,
        input: Option<Self::Symbol>,
    ) -> Result<ParserAction<Self::State, Self::Symbol, Self::Reduce>, Self::Error> {
        (**self).action(current, input)
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
#[non_exhaustive]
pub enum ParserAction<TState, TSymbol, TReduce> {
    Shift(TState),
    Reduce(TReduce, TSymbol, usize),
    Accept,
}

pub trait Token<TSym> {
    fn as_symbol(&self) -> TSym;
}

/// The parser type based on generated definition.
#[derive(Debug)]
pub struct Parser<TDef, TTok>
where
    TDef: ParserDefinition,
    TTok: Token<TDef::Symbol>,
{
    definition: TDef,
    state_stack: Vec<TDef::State>,
    item_stack: Vec<StackItem<TTok, TDef::Symbol>>,
    parser_state: ParserState,
    peeked_token: Option<TTok>,
}

#[derive(Debug)]
enum StackItem<TTok, TSym> {
    T(TTok),
    N(TSym),
}

#[derive(Debug)]
enum ParserState {
    Reading,
    PendingGoto,
    Accepted,
}

impl<TDef, TTok> Parser<TDef, TTok>
where
    TDef: ParserDefinition,
    TTok: Token<TDef::Symbol>,
{
    /// Create an instance of `Parser` using the specified parser definition.
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
    /// until it matches a certain syntax rule.
    pub fn next_event<I, E>(
        &mut self,
        tokens: &mut I,
        args: &mut Vec<ParseItem<TTok, TDef::Symbol>>,
    ) -> Result<ParseEvent<TDef>, ParseError<E, TDef::Error>>
    where
        I: Iterator<Item = Result<TTok, E>>,
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
                    StackItem::N(s) => Some(*s),
                    StackItem::T(t) => Some(t.as_symbol()),
                },
                _ => {
                    if self.peeked_token.is_none() {
                        self.peeked_token = tokens.next().transpose().map_err(ParseError::Lexer)?;
                    }
                    self.peeked_token.as_ref().map(|t| t.as_symbol())
                }
            };

            match self
                .definition
                .action(*current, input)
                .map_err(ParseError::ParserDef)?
            {
                ParserAction::Shift(n) => {
                    if !matches!(self.parser_state, ParserState::PendingGoto) {
                        let t = match self.peeked_token.take() {
                            Some(t) => t,
                            None => tokens
                                .next()
                                .transpose()
                                .map_err(ParseError::Lexer)?
                                .ok_or_else(|| ParseError::UnexpectedEOI)?,
                        };
                        self.item_stack.push(StackItem::T(t));
                    }

                    self.parser_state = ParserState::Reading;
                    self.state_stack.push(n);
                    continue;
                }

                ParserAction::Reduce(reduce, lhs, n) => {
                    args.clear();
                    for _ in 0..n {
                        self.state_stack.pop();
                        let item = self
                            .item_stack
                            .pop()
                            .ok_or_else(|| ParseError::EmptyItemStack)?;
                        let arg = match item {
                            StackItem::T(token) => ParseItem::Terminal(token),
                            StackItem::N(symbol) => ParseItem::Nonterminal(symbol),
                        };
                        args.push(arg);
                    }
                    args.reverse();

                    self.item_stack.push(StackItem::N(lhs));
                    self.parser_state = ParserState::PendingGoto;

                    return Ok(ParseEvent::Reduce(reduce));
                }

                ParserAction::Accept => {
                    let item = self
                        .item_stack
                        .pop()
                        .ok_or_else(|| ParseError::EmptyItemStack)?;
                    let arg = match item {
                        StackItem::T(token) => ParseItem::Terminal(token),
                        StackItem::N(symbol) => ParseItem::Nonterminal(symbol),
                    };
                    args.clear();
                    args.push(arg);

                    self.parser_state = ParserState::Accepted;
                    return Ok(ParseEvent::Accept);
                }
            }
        }
    }
}

#[derive(Debug)]
#[non_exhaustive]
pub enum ParseItem<TTok, TSym> {
    Terminal(TTok),
    Nonterminal(TSym),
}

#[derive(Debug)]
pub enum ParseEvent<TDef>
where
    TDef: ParserDefinition,
{
    Reduce(TDef::Reduce),
    Accept,
}

#[derive(Debug, thiserror::Error)]
pub enum ParseError<L, D> {
    #[error("from lexer: {}", 0)]
    Lexer(L),

    #[error("from parser definition: {}", 0)]
    ParserDef(D),

    #[error("unexpected EOI")]
    UnexpectedEOI,

    #[error("empty node stack")]
    EmptyNodeStack,

    #[error("empty item stack")]
    EmptyItemStack,
}

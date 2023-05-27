//! Parser.

pub trait ParserDefinition {
    type State: Copy;
    type Symbol: Copy;
    type Reduce;
    type Action: ParserAction<State = Self::State, Symbol = Self::Symbol, Reduce = Self::Reduce>;

    fn initial_state(&self) -> Self::State;
    fn action(&self, current: Self::State, input: Option<Self::Symbol>) -> Self::Action;
}

impl<T: ?Sized> ParserDefinition for &T
where
    T: ParserDefinition,
{
    type State = T::State;
    type Symbol = T::Symbol;
    type Reduce = T::Reduce;
    type Action = T::Action;

    fn initial_state(&self) -> Self::State {
        (**self).initial_state()
    }

    fn action(&self, current: Self::State, input: Option<Self::Symbol>) -> Self::Action {
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
    type Action = T::Action;

    fn initial_state(&self) -> Self::State {
        (**self).initial_state()
    }

    fn action(&self, current: Self::State, input: Option<Self::Symbol>) -> Self::Action {
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
    type Action = T::Action;

    fn initial_state(&self) -> Self::State {
        (**self).initial_state()
    }

    fn action(&self, current: Self::State, input: Option<Self::Symbol>) -> Self::Action {
        (**self).action(current, input)
    }
}

pub trait ParserAction {
    type State: Copy;
    type Symbol: Copy;
    type Reduce;

    fn into_kind(self) -> ParserActionKind<Self::State, Self::Symbol, Self::Reduce>;
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ParserActionKind<TState, TSymbol, TReduce> {
    Shift(TState),
    Reduce(TReduce, TSymbol, usize),
    Accept,
}

pub trait Token<TSym> {
    fn as_symbol(&self) -> TSym;
}

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

    /// a
    pub fn next_event<I>(
        &mut self,
        tokens: &mut I,
        args: &mut Vec<ParseItem<TTok, TDef::Symbol>>,
    ) -> ParseEvent<TDef>
    where
        I: Iterator<Item = TTok>,
    {
        loop {
            let current = self.state_stack.last().unwrap();

            let input = match self.parser_state {
                ParserState::PendingGoto => match self.item_stack.last().unwrap() {
                    StackItem::N(s) => Some(*s),
                    StackItem::T(t) => Some(t.as_symbol()),
                },
                _ => {
                    if self.peeked_token.is_none() {
                        self.peeked_token = tokens.next();
                    }
                    self.peeked_token.as_ref().map(|t| t.as_symbol())
                }
            };

            match self.definition.action(*current, input).into_kind() {
                ParserActionKind::Shift(n) => {
                    if !matches!(self.parser_state, ParserState::PendingGoto) {
                        let t = match self.peeked_token.take() {
                            Some(t) => t,
                            None => tokens.next().expect("unexpected EOI"),
                        };
                        self.item_stack.push(StackItem::T(t));
                    }

                    self.parser_state = ParserState::Reading;
                    self.state_stack.push(n);
                    continue;
                }
                ParserActionKind::Reduce(reduce, lhs, n) => {
                    args.clear();
                    for _ in 0..n {
                        self.state_stack.pop();
                        let item = self.item_stack.pop().unwrap();
                        let arg = match item {
                            StackItem::T(token) => ParseItem::Terminal(token),
                            StackItem::N(symbol) => ParseItem::Nonterminal(symbol),
                        };
                        args.push(arg);
                    }
                    args.reverse();

                    self.item_stack.push(StackItem::N(lhs));
                    self.parser_state = ParserState::PendingGoto;

                    return ParseEvent::Reduce(reduce);
                }
                ParserActionKind::Accept => {
                    self.parser_state = ParserState::Accepted;
                    let item = self.item_stack.pop().expect("empty result stack");
                    let arg = match item {
                        StackItem::T(token) => ParseItem::Terminal(token),
                        StackItem::N(symbol) => ParseItem::Nonterminal(symbol),
                    };
                    args.clear();
                    args.push(arg);
                    return ParseEvent::Accept;
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

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

    fn into_kind(self) -> ParserActionKind<Self>;
}

pub enum ParserActionKind<A: ParserAction + ?Sized> {
    Shift(A::State),
    Reduce(A::Reduce, A::Symbol, usize),
    Accept,
}

#[derive(Debug)]
pub struct Parser<T, D>
where
    D: ParserDefinition,
{
    def: D,
    state_stack: Vec<D::State>,
    item_stack: Vec<StackItem<T, D>>,
    parser_state: ParserState,
    peeked: Option<(D::Symbol, T)>,
}

#[derive(Debug)]
enum ParserState {
    Reading,
    PendingGoto,
    Accepted,
}

impl<T, D> Parser<T, D>
where
    D: ParserDefinition,
{
    pub fn new(def: D) -> Self {
        let initial_state = def.initial_state();
        Self {
            def,
            state_stack: vec![initial_state],
            item_stack: vec![],
            parser_state: ParserState::Reading,
            peeked: None,
        }
    }

    /// a
    pub fn next_event<I>(&mut self, tokens: &mut I) -> ParseEvent<T, D>
    where
        I: Iterator<Item = (D::Symbol, T)>,
    {
        loop {
            let current = self.state_stack.last().unwrap();

            let input = match self.parser_state {
                ParserState::PendingGoto => match self.item_stack.last().unwrap() {
                    StackItem::N(t) => Some(*t),
                    StackItem::T(t, _) => Some(*t),
                },
                _ => {
                    if self.peeked.is_none() {
                        self.peeked = tokens.next();
                    }
                    self.peeked.as_ref().map(|(id, _)| *id)
                }
            };

            match self.def.action(*current, input).into_kind() {
                ParserActionKind::Shift(n) => {
                    if !matches!(self.parser_state, ParserState::PendingGoto) {
                        let (id, t) = match self.peeked.take() {
                            Some(t) => t,
                            None => tokens.next().expect("unexpected EOI"),
                        };
                        self.item_stack.push(StackItem::T(id, t));
                    }

                    self.parser_state = ParserState::Reading;
                    self.state_stack.push(n);
                    continue;
                }
                ParserActionKind::Reduce(context, lhs, n) => {
                    let mut args = vec![];
                    for _ in 0..n {
                        self.state_stack.pop();
                        let item = self.item_stack.pop().unwrap();
                        let arg = match item {
                            StackItem::T(_, token) => ParseItem::T(token),
                            StackItem::N(_t) => ParseItem::N,
                        };
                        args.push(arg);
                    }
                    args.reverse();

                    self.item_stack.push(StackItem::N(lhs));
                    self.parser_state = ParserState::PendingGoto;

                    return ParseEvent::Reduce(context, args);
                }
                ParserActionKind::Accept => {
                    self.parser_state = ParserState::Accepted;
                    let item = self.item_stack.pop().expect("empty result stack");
                    let arg = match item {
                        StackItem::N(_t) => ParseItem::N,
                        StackItem::T(_, token) => ParseItem::T(token),
                    };
                    return ParseEvent::Accept(arg);
                }
            }
        }
    }
}

#[derive(Debug)]
enum StackItem<T, D>
where
    D: ParserDefinition,
{
    T(D::Symbol, T),
    N(D::Symbol),
}

#[derive(Debug)]
pub enum ParseItem<T> {
    T(T),
    N,
}

#[derive(Debug)]
pub enum ParseEvent<T, D>
where
    D: ParserDefinition,
{
    Reduce(D::Reduce, Vec<ParseItem<T>>),
    Accept(ParseItem<T>),
}

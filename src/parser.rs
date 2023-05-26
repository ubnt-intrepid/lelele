//! Parser.

pub trait ParserDefinition {
    type NodeID: Copy;
    type SymbolID: Copy;
    type Reduce;
    type Action: ParserAction<
        NodeID = Self::NodeID,
        SymbolID = Self::SymbolID,
        Reduce = Self::Reduce,
    >;

    fn initial_state(&self) -> Self::NodeID;
    fn action(&self, current: Self::NodeID, input: Option<Self::SymbolID>) -> Self::Action;
}
impl<T: ?Sized> ParserDefinition for &T
where
    T: ParserDefinition,
{
    type NodeID = T::NodeID;
    type SymbolID = T::SymbolID;
    type Reduce = T::Reduce;
    type Action = T::Action;

    fn initial_state(&self) -> Self::NodeID {
        (**self).initial_state()
    }

    fn action(&self, current: Self::NodeID, input: Option<Self::SymbolID>) -> Self::Action {
        (**self).action(current, input)
    }
}
impl<T: ?Sized> ParserDefinition for std::rc::Rc<T>
where
    T: ParserDefinition,
{
    type NodeID = T::NodeID;
    type SymbolID = T::SymbolID;
    type Reduce = T::Reduce;
    type Action = T::Action;

    fn initial_state(&self) -> Self::NodeID {
        (**self).initial_state()
    }

    fn action(&self, current: Self::NodeID, input: Option<Self::SymbolID>) -> Self::Action {
        (**self).action(current, input)
    }
}
impl<T: ?Sized> ParserDefinition for std::sync::Arc<T>
where
    T: ParserDefinition,
{
    type NodeID = T::NodeID;
    type SymbolID = T::SymbolID;
    type Reduce = T::Reduce;
    type Action = T::Action;

    fn initial_state(&self) -> Self::NodeID {
        (**self).initial_state()
    }

    fn action(&self, current: Self::NodeID, input: Option<Self::SymbolID>) -> Self::Action {
        (**self).action(current, input)
    }
}

pub trait ParserAction {
    type NodeID: Copy;
    type SymbolID: Copy;
    type Reduce;

    fn into_kind(self) -> ParserActionKind<Self>;
}

pub enum ParserActionKind<A: ParserAction + ?Sized> {
    Shift(A::NodeID),
    Reduce(A::Reduce, A::SymbolID, usize),
    Accept,
}

#[derive(Debug)]
pub struct Parser<T, D>
where
    D: ParserDefinition,
{
    def: D,
    nodes: Vec<D::NodeID>,
    item_stack: Vec<StackItem<T, D>>,
    state: ParserState,
    peeked: Option<(D::SymbolID, T)>,
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
        let initial_node = def.initial_state();
        Self {
            def,
            nodes: vec![initial_node],
            item_stack: vec![],
            state: ParserState::Reading,
            peeked: None,
        }
    }

    pub fn next_event<I>(&mut self, tokens: &mut I) -> ParseEvent<T, D>
    where
        I: Iterator<Item = (D::SymbolID, T)>,
    {
        loop {
            let current = self.nodes.last().unwrap();

            let input = match self.state {
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
                    if !matches!(self.state, ParserState::PendingGoto) {
                        let (id, t) = match self.peeked.take() {
                            Some(t) => t,
                            None => tokens.next().expect("unexpected EOI"),
                        };
                        self.item_stack.push(StackItem::T(id, t));
                    }

                    self.state = ParserState::Reading;
                    self.nodes.push(n);
                    continue;
                }
                ParserActionKind::Reduce(context, lhs, n) => {
                    let mut args = vec![];
                    for _ in 0..n {
                        self.nodes.pop();
                        let item = self.item_stack.pop().unwrap();
                        let arg = match item {
                            StackItem::T(_, token) => ParseItem::T(token),
                            StackItem::N(_t) => ParseItem::N,
                        };
                        args.push(arg);
                    }
                    args.reverse();

                    self.item_stack.push(StackItem::N(lhs));
                    self.state = ParserState::PendingGoto;

                    return ParseEvent::Reduce(context, args);
                }
                ParserActionKind::Accept => {
                    self.state = ParserState::Accepted;
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
    T(D::SymbolID, T),
    N(D::SymbolID),
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

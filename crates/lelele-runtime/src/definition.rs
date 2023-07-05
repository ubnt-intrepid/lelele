//! Parser definition.

/// The trait that represents an LR(1) parser definition generated from a particular CFG.
///
/// The main responsibility of types implementing this trait is to tell the parser engine
/// what the next action should be, given a particular state and lookahead symbol(s).
pub trait ParserDef {
    type StateIndex: Copy;
    type TerminalIndex: Copy;
    type NonterminalIndex: Copy;

    /// Return the initial state number.
    fn initial_state(&self) -> Self::StateIndex;

    /// Suggests the next action that the parser engine should take for
    /// the current state and lookahead symbol.
    fn action(
        &self,
        current: Self::StateIndex,
        lookahead: Terminal<Self::TerminalIndex>,
    ) -> ParseAction<Self>;

    ///
    fn goto(&self, current: Self::StateIndex, symbol: Self::NonterminalIndex) -> Self::StateIndex;

    ///
    fn expected_terminals(&self, current: Self::StateIndex) -> &[Terminal<Self::TerminalIndex>];
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Terminal<T> {
    T(T),
    EOI,
}

pub type ParseAction<TDef> =
    ParseActionKind<<TDef as ParserDef>::StateIndex, <TDef as ParserDef>::NonterminalIndex>;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ParseActionKind<TState, TNonterminal> {
    Shift(TState),
    Reduce(TNonterminal, usize),
    Accept,
    Fail,
}

impl<T: ?Sized> ParserDef for &T
where
    T: ParserDef,
{
    type StateIndex = T::StateIndex;
    type TerminalIndex = T::TerminalIndex;
    type NonterminalIndex = T::NonterminalIndex;

    fn initial_state(&self) -> Self::StateIndex {
        (**self).initial_state()
    }

    #[inline]
    fn action(
        &self,
        current: Self::StateIndex,
        lookahead: Terminal<Self::TerminalIndex>,
    ) -> ParseAction<Self> {
        (**self).action(current, lookahead)
    }

    #[inline]
    fn goto(&self, current: Self::StateIndex, symbol: Self::NonterminalIndex) -> Self::StateIndex {
        (**self).goto(current, symbol)
    }

    #[inline]
    fn expected_terminals(&self, current: Self::StateIndex) -> &[Terminal<Self::TerminalIndex>] {
        (**self).expected_terminals(current)
    }
}

impl<T: ?Sized> ParserDef for Box<T>
where
    T: ParserDef,
{
    type StateIndex = T::StateIndex;
    type TerminalIndex = T::TerminalIndex;
    type NonterminalIndex = T::NonterminalIndex;

    fn initial_state(&self) -> Self::StateIndex {
        (**self).initial_state()
    }

    #[inline]
    fn action(
        &self,
        current: Self::StateIndex,
        lookahead: Terminal<Self::TerminalIndex>,
    ) -> ParseAction<Self> {
        (**self).action(current, lookahead)
    }

    #[inline]
    fn goto(&self, current: Self::StateIndex, symbol: Self::NonterminalIndex) -> Self::StateIndex {
        (**self).goto(current, symbol)
    }

    #[inline]
    fn expected_terminals(&self, current: Self::StateIndex) -> &[Terminal<Self::TerminalIndex>] {
        (**self).expected_terminals(current)
    }
}

impl<T: ?Sized> ParserDef for std::rc::Rc<T>
where
    T: ParserDef,
{
    type StateIndex = T::StateIndex;
    type TerminalIndex = T::TerminalIndex;
    type NonterminalIndex = T::NonterminalIndex;

    fn initial_state(&self) -> Self::StateIndex {
        (**self).initial_state()
    }

    #[inline]
    fn action(
        &self,
        current: Self::StateIndex,
        lookahead: Terminal<Self::TerminalIndex>,
    ) -> ParseAction<Self> {
        (**self).action(current, lookahead)
    }

    #[inline]
    fn goto(&self, current: Self::StateIndex, symbol: Self::NonterminalIndex) -> Self::StateIndex {
        (**self).goto(current, symbol)
    }

    #[inline]
    fn expected_terminals(&self, current: Self::StateIndex) -> &[Terminal<Self::TerminalIndex>] {
        (**self).expected_terminals(current)
    }
}

impl<T: ?Sized> ParserDef for std::sync::Arc<T>
where
    T: ParserDef,
{
    type StateIndex = T::StateIndex;
    type TerminalIndex = T::TerminalIndex;
    type NonterminalIndex = T::NonterminalIndex;

    fn initial_state(&self) -> Self::StateIndex {
        (**self).initial_state()
    }

    #[inline]
    fn action(
        &self,
        current: Self::StateIndex,
        lookahead: Terminal<Self::TerminalIndex>,
    ) -> ParseAction<Self> {
        (**self).action(current, lookahead)
    }

    #[inline]
    fn goto(&self, current: Self::StateIndex, symbol: Self::NonterminalIndex) -> Self::StateIndex {
        (**self).goto(current, symbol)
    }

    #[inline]
    fn expected_terminals(&self, current: Self::StateIndex) -> &[Terminal<Self::TerminalIndex>] {
        (**self).expected_terminals(current)
    }
}

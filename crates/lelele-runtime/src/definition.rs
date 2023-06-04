//! Parser definition.

/// The trait for abstracting the generated LR(1) parse table.
pub trait ParseTable {
    /// The number to identify the state of LR(1) automaton.
    type State: Copy;

    /// The number to identify the terminal/nonterminal symbols.
    type Symbol: Copy;

    /// The context value corresponding to the matched production rule.
    type Reduce;

    /// Return the initial state number.
    fn initial_state(&self) -> Self::State;

    /// Return the action corresponding to the specified state number and
    /// lookahead symbol.
    ///
    /// If there is no lookahead symbol, a `None` is passsed as the end of input.
    fn action(
        &self,
        current: Self::State,
        lookahead: Option<Self::Symbol>,
    ) -> ParseAction<Self::State, Self::Symbol, Self::Reduce>;
}

impl<T: ?Sized> ParseTable for &T
where
    T: ParseTable,
{
    type State = T::State;
    type Symbol = T::Symbol;
    type Reduce = T::Reduce;

    fn initial_state(&self) -> Self::State {
        (**self).initial_state()
    }

    fn action(
        &self,
        current: Self::State,
        lookahead: Option<Self::Symbol>,
    ) -> ParseAction<Self::State, Self::Symbol, Self::Reduce> {
        (**self).action(current, lookahead)
    }
}

impl<T: ?Sized> ParseTable for std::rc::Rc<T>
where
    T: ParseTable,
{
    type State = T::State;
    type Symbol = T::Symbol;
    type Reduce = T::Reduce;

    fn initial_state(&self) -> Self::State {
        (**self).initial_state()
    }

    fn action(
        &self,
        current: Self::State,
        lookahead: Option<Self::Symbol>,
    ) -> ParseAction<Self::State, Self::Symbol, Self::Reduce> {
        (**self).action(current, lookahead)
    }
}

impl<T: ?Sized> ParseTable for std::sync::Arc<T>
where
    T: ParseTable,
{
    type State = T::State;
    type Symbol = T::Symbol;
    type Reduce = T::Reduce;

    fn initial_state(&self) -> Self::State {
        (**self).initial_state()
    }

    fn action(
        &self,
        current: Self::State,
        input: Option<Self::Symbol>,
    ) -> ParseAction<Self::State, Self::Symbol, Self::Reduce> {
        (**self).action(current, input)
    }
}

#[derive(Debug, Copy, Clone)]
#[non_exhaustive]
pub enum ParseAction<TState, TSymbol, TReduce> {
    Shift(TState),
    Reduce(TReduce, TSymbol, usize),
    Accept,
    Error(ParseActionError),
}

#[derive(Debug, Copy, Clone, thiserror::Error)]
pub enum ParseActionError {
    #[error("incorrect state")]
    IncorrectState,

    #[error("incorrect symbol")]
    IncorrectSymbol,
}

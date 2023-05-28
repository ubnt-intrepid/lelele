//! Parser definition.

pub trait ParserDefinition {
    type State: Copy;
    type Symbol: Copy;
    type Reduce;
    fn initial_state(&self) -> Self::State;
    fn action(
        &self,
        current: Self::State,
        input: Option<Self::Symbol>,
    ) -> Result<ParserAction<Self::State, Self::Symbol, Self::Reduce>, ParserActionError>;
}

impl<T: ?Sized> ParserDefinition for &T
where
    T: ParserDefinition,
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
    ) -> Result<ParserAction<Self::State, Self::Symbol, Self::Reduce>, ParserActionError> {
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

    fn initial_state(&self) -> Self::State {
        (**self).initial_state()
    }

    fn action(
        &self,
        current: Self::State,
        input: Option<Self::Symbol>,
    ) -> Result<ParserAction<Self::State, Self::Symbol, Self::Reduce>, ParserActionError> {
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

    fn initial_state(&self) -> Self::State {
        (**self).initial_state()
    }

    fn action(
        &self,
        current: Self::State,
        input: Option<Self::Symbol>,
    ) -> Result<ParserAction<Self::State, Self::Symbol, Self::Reduce>, ParserActionError> {
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

#[derive(Debug, thiserror::Error)]
pub enum ParserActionError {
    #[error("incorrect state")]
    IncorrectState,

    #[error("incorrect symbol")]
    IncorrectSymbol,
}

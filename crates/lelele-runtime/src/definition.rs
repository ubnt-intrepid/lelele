//! Parser definition.

/// The trait that represents an LR(1) parser definition generated from a particular CFG.
///
/// The main responsibility of types implementing this trait is to tell the parser engine
/// what the next action should be, given a particular state and lookahead symbol(s).
pub trait ParserDef {
    type State: Copy;
    type Token: Copy;
    type Symbol: Copy;

    /// Return the initial state number.
    fn initial_state(&self) -> Self::State;

    /// Suggests the next action that the parser engine should take for
    /// the current state and lookahead symbol.
    fn action<TAction>(
        &self,
        current: Self::State,
        lookahead: Option<Self::Token>,
        action: TAction,
    ) -> Result<TAction::Ok, TAction::Error>
    where
        TAction: ParseAction<State = Self::State, Token = Self::Token, Symbol = Self::Symbol>;

    fn goto(&self, current: Self::State, symbol: Self::Symbol) -> Option<Self::State>;
}

pub trait ParseAction {
    type State: Copy;
    type Token: Copy;
    type Symbol: Copy;

    type Ok;
    type Error;

    ///
    fn shift(self, next: Self::State) -> Result<Self::Ok, Self::Error>;

    ///
    fn reduce(self, s: Self::Symbol, n: usize) -> Result<Self::Ok, Self::Error>;

    ///
    fn fail<I>(self, expected_tokens: I) -> Result<Self::Ok, Self::Error>
    where
        I: IntoIterator<Item = Self::Token>;
}

impl<T: ?Sized> ParserDef for &T
where
    T: ParserDef,
{
    type State = T::State;
    type Token = T::Token;
    type Symbol = T::Symbol;

    fn initial_state(&self) -> Self::State {
        (**self).initial_state()
    }

    #[inline]
    fn action<TCtx>(
        &self,
        current: Self::State,
        lookahead: Option<Self::Token>,
        cx: TCtx,
    ) -> Result<TCtx::Ok, TCtx::Error>
    where
        TCtx: ParseAction<State = Self::State, Token = Self::Token, Symbol = Self::Symbol>,
    {
        (**self).action(current, lookahead, cx)
    }

    #[inline]
    fn goto(&self, current: Self::State, symbol: Self::Symbol) -> Option<Self::State> {
        (**self).goto(current, symbol)
    }
}

impl<T: ?Sized> ParserDef for std::rc::Rc<T>
where
    T: ParserDef,
{
    type State = T::State;
    type Token = T::Token;
    type Symbol = T::Symbol;

    fn initial_state(&self) -> Self::State {
        (**self).initial_state()
    }

    #[inline]
    fn action<TCtx>(
        &self,
        current: Self::State,
        lookahead: Option<Self::Token>,
        cx: TCtx,
    ) -> Result<TCtx::Ok, TCtx::Error>
    where
        TCtx: ParseAction<State = Self::State, Token = Self::Token, Symbol = Self::Symbol>,
    {
        (**self).action(current, lookahead, cx)
    }

    #[inline]
    fn goto(&self, current: Self::State, symbol: Self::Symbol) -> Option<Self::State> {
        (**self).goto(current, symbol)
    }
}

impl<T: ?Sized> ParserDef for std::sync::Arc<T>
where
    T: ParserDef,
{
    type State = T::State;
    type Token = T::Token;
    type Symbol = T::Symbol;

    fn initial_state(&self) -> Self::State {
        (**self).initial_state()
    }

    #[inline]
    fn action<TCtx>(
        &self,
        current: Self::State,
        lookahead: Option<Self::Token>,
        cx: TCtx,
    ) -> Result<TCtx::Ok, TCtx::Error>
    where
        TCtx: ParseAction<State = Self::State, Token = Self::Token, Symbol = Self::Symbol>,
    {
        (**self).action(current, lookahead, cx)
    }

    #[inline]
    fn goto(&self, current: Self::State, symbol: Self::Symbol) -> Option<Self::State> {
        (**self).goto(current, symbol)
    }
}

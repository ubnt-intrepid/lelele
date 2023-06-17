//! Parser definition.

/// The trait for abstracting the generated LR(1) parse table.
pub trait ParserDef {
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
    fn action<TCtx>(&self, cx: TCtx) -> Result<TCtx::Ok, TCtx::Err>
    where
        TCtx: ParseContext<State = Self::State, Symbol = Self::Symbol, Reduce = Self::Reduce>;
}

pub trait ParseContext {
    type State: Copy;
    type Symbol: Copy;
    type Reduce;

    type Ok;
    type Err;

    fn current_state(&self) -> Self::State;
    fn lookahead(&self) -> Option<Self::Symbol>;
    fn shift(&mut self, next: Self::State) -> Result<(), Self::Err>;
    fn reduce(&mut self, r: Self::Reduce, s: Self::Symbol, n: usize) -> Result<(), Self::Err>;
    fn accept(&mut self) -> Result<(), Self::Err>;
    fn error(&mut self, reason: &str) -> Result<(), Self::Err>;
    fn end(self) -> Result<Self::Ok, Self::Err>;
}

impl<T: ?Sized> ParserDef for &T
where
    T: ParserDef,
{
    type State = T::State;
    type Symbol = T::Symbol;
    type Reduce = T::Reduce;

    fn initial_state(&self) -> Self::State {
        (**self).initial_state()
    }

    #[inline]
    fn action<TCtx>(&self, cx: TCtx) -> Result<TCtx::Ok, TCtx::Err>
    where
        TCtx: ParseContext<State = Self::State, Symbol = Self::Symbol, Reduce = Self::Reduce>,
    {
        (**self).action(cx)
    }
}

impl<T: ?Sized> ParserDef for std::rc::Rc<T>
where
    T: ParserDef,
{
    type State = T::State;
    type Symbol = T::Symbol;
    type Reduce = T::Reduce;

    fn initial_state(&self) -> Self::State {
        (**self).initial_state()
    }

    #[inline]
    fn action<TCtx>(&self, cx: TCtx) -> Result<TCtx::Ok, TCtx::Err>
    where
        TCtx: ParseContext<State = Self::State, Symbol = Self::Symbol, Reduce = Self::Reduce>,
    {
        (**self).action(cx)
    }
}

impl<T: ?Sized> ParserDef for std::sync::Arc<T>
where
    T: ParserDef,
{
    type State = T::State;
    type Symbol = T::Symbol;
    type Reduce = T::Reduce;

    fn initial_state(&self) -> Self::State {
        (**self).initial_state()
    }

    #[inline]
    fn action<TCtx>(&self, cx: TCtx) -> Result<TCtx::Ok, TCtx::Err>
    where
        TCtx: ParseContext<State = Self::State, Symbol = Self::Symbol, Reduce = Self::Reduce>,
    {
        (**self).action(cx)
    }
}

//! Parser definition.

/// The trait that represents an LR(1) parser definition generated from a particular CFG.
///
/// The main responsibility of types implementing this trait is to tell the parser engine
/// what the next action should be, given a particular state and lookahead symbol(s).
///
/// Note that multiple actions may be suggested for a single state and lookahead, due to
/// the ambiguity of grammar definition. This trait is designed so that such grammars
/// can still be parsed (using algorithms such as GLR parsing).
pub trait ParserDef {
    /// The number to identify the state of LR(1) automaton.
    type State: Copy;

    type Token: Copy;

    /// The number to identify the terminal/nonterminal symbols.
    type Symbol: Copy;

    /// The context value corresponding to the matched production rule.
    type Reduce;

    /// Return the initial state number.
    fn initial_state(&self) -> Self::State;

    /// Suggests the next action that the parser engine should take for
    /// the current state and lookahead symbol.
    fn action<TCtx>(&self, cx: TCtx) -> Result<TCtx::Ok, TCtx::Err>
    where
        TCtx: ParseContext<
            State = Self::State,
            Token = Self::Token,
            Symbol = Self::Symbol,
            Reduce = Self::Reduce,
        >;
}

pub trait ParseContext {
    type State: Copy;
    type Token: Copy;
    type Symbol: Copy;
    type Reduce;

    type Ok;
    type Err;

    /// Return the current state in this context.
    fn current_state(&self) -> Self::State;

    /// Return the most recent lookahead symbol in this context.
    fn lookahead(&self) -> Lookahead<Self::Token, Self::Symbol>;

    /// Tells the parser engine that the next action is "shift to state `next`".
    fn shift(&mut self, next: Self::State) -> Result<(), Self::Err>;

    /// Tells the parser engine that the next action is "reduce to the production rule `r`".
    fn reduce(&mut self, r: Self::Reduce, s: Self::Symbol, n: usize) -> Result<(), Self::Err>;

    /// Tells the parser engine that the next action is "reduce to the toplevel symbol".
    fn accept(&mut self) -> Result<(), Self::Err>;

    fn error(&mut self, reason: &str) -> Result<(), Self::Err>;

    /// Complete the instructions for this context.
    fn end(self) -> Result<Self::Ok, Self::Err>;
}

impl<T: ?Sized> ParserDef for &T
where
    T: ParserDef,
{
    type State = T::State;
    type Token = T::Token;
    type Symbol = T::Symbol;
    type Reduce = T::Reduce;

    fn initial_state(&self) -> Self::State {
        (**self).initial_state()
    }

    #[inline]
    fn action<TCtx>(&self, cx: TCtx) -> Result<TCtx::Ok, TCtx::Err>
    where
        TCtx: ParseContext<
            State = Self::State,
            Token = Self::Token,
            Symbol = Self::Symbol,
            Reduce = Self::Reduce,
        >,
    {
        (**self).action(cx)
    }
}

impl<T: ?Sized> ParserDef for std::rc::Rc<T>
where
    T: ParserDef,
{
    type State = T::State;
    type Token = T::Token;
    type Symbol = T::Symbol;
    type Reduce = T::Reduce;

    fn initial_state(&self) -> Self::State {
        (**self).initial_state()
    }

    #[inline]
    fn action<TCtx>(&self, cx: TCtx) -> Result<TCtx::Ok, TCtx::Err>
    where
        TCtx: ParseContext<
            State = Self::State,
            Token = Self::Token,
            Symbol = Self::Symbol,
            Reduce = Self::Reduce,
        >,
    {
        (**self).action(cx)
    }
}

impl<T: ?Sized> ParserDef for std::sync::Arc<T>
where
    T: ParserDef,
{
    type State = T::State;
    type Token = T::Token;
    type Symbol = T::Symbol;
    type Reduce = T::Reduce;

    fn initial_state(&self) -> Self::State {
        (**self).initial_state()
    }

    #[inline]
    fn action<TCtx>(&self, cx: TCtx) -> Result<TCtx::Ok, TCtx::Err>
    where
        TCtx: ParseContext<
            State = Self::State,
            Token = Self::Token,
            Symbol = Self::Symbol,
            Reduce = Self::Reduce,
        >,
    {
        (**self).action(cx)
    }
}

#[derive(Debug, Copy, Clone)]
#[non_exhaustive]
pub enum Lookahead<T, N> {
    T(T),
    N(N),
    Eoi,
}

pub mod p {
    include!(concat!(env!("OUT_DIR"), "/g_simple1/parser.rs"));
}
#[allow(unused_imports)]
use lelele_runtime::parser::{ParseEvent::*, ParseItem::*};
use std::convert::Infallible;

#[derive(Debug)]
pub struct Token(p::SymbolID);
impl lelele_runtime::parser::Token<p::SymbolID> for Token {
    fn as_symbol(&self) -> p::SymbolID {
        self.0
    }
}

pub fn tokens<I>(iter: I) -> impl Iterator<Item = Result<Token, Infallible>>
where
    I: IntoIterator<Item = p::SymbolID>,
{
    iter.into_iter().map(|t| Ok(Token(t)))
}

#[allow(unused_macros)]
macro_rules! assert_matches {
    ($($t:tt)*) => { assert!(matches!($($t)*)) };
}

#[test]
fn case1() {
    let mut tokens = tokens([p::SymbolID::NUM, p::SymbolID::EQUAL, p::SymbolID::NUM]);
    let mut parser = p::parser::<Token>();
    let mut args = vec![];

    let event = parser.next_event(&mut tokens, &mut args).unwrap();
    assert_matches!(
        (event, &args[..]),
        (Reduce(p::RuleID::T0), [T(Token(p::SymbolID::NUM))])
    );

    let event = parser.next_event(&mut tokens, &mut args).unwrap();
    assert_matches!(
        (event, &args[..]),
        (Reduce(p::RuleID::E1), [N(p::SymbolID::T)])
    );

    let event = parser.next_event(&mut tokens, &mut args).unwrap();
    assert_matches!(
        (event, &args[..]),
        (Reduce(p::RuleID::T0), [T(Token(p::SymbolID::NUM))])
    );

    let event = parser.next_event(&mut tokens, &mut args).unwrap();
    assert_matches!(
        (event, &args[..]),
        (Reduce(p::RuleID::E1), [N(p::SymbolID::T)])
    );

    let event = parser.next_event(&mut tokens, &mut args).unwrap();
    assert_matches!(
        (event, &args[..]),
        (
            Reduce(p::RuleID::A0),
            [
                N(p::SymbolID::E),
                T(Token(p::SymbolID::EQUAL)),
                N(p::SymbolID::E)
            ]
        )
    );

    let event = parser.next_event(&mut tokens, &mut args).unwrap();
    assert_matches!((event, &args[..]), (Accept, [N(p::SymbolID::A)]));
}

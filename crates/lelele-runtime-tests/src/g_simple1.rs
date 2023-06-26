pub mod p {
    include!(concat!(env!("OUT_DIR"), "/g_simple1/parser.rs"));
}
#[allow(unused_imports)]
use lelele_runtime::parser::{ParseEvent::*, ParseItem::*};

#[allow(unused_macros)]
macro_rules! assert_matches {
    ($($t:tt)*) => { assert!(matches!($($t)*)) };
}

#[test]
fn case1() {
    let mut parser = p::parser::<p::TokenID>();

    assert_matches!(parser.next_event(), Ok(InputNeeded));
    parser.offer_token(p::TokenID::NUM);

    assert_matches!(parser.next_event(), Ok(Shifting(Some(&p::TokenID::NUM))));

    assert_matches!(parser.next_event(), Ok(InputNeeded));
    parser.offer_token(p::TokenID::EQUAL);

    assert_matches!(
        parser.next_event(),
        Ok(AboutToReduce(p::RuleID::T0, [T(Some(p::TokenID::NUM))]))
    );

    assert_matches!(
        parser.next_event(),
        Ok(AboutToReduce(p::RuleID::E1, [N(p::SymbolID::T)]))
    );

    assert_matches!(parser.next_event(), Ok(Shifting(Some(&p::TokenID::EQUAL))));

    assert_matches!(parser.next_event(), Ok(InputNeeded));
    parser.offer_token(p::TokenID::NUM);

    assert_matches!(parser.next_event(), Ok(Shifting(Some(&p::TokenID::NUM))));

    assert_matches!(parser.next_event(), Ok(InputNeeded));
    parser.offer_eoi();

    assert_matches!(
        parser.next_event(),
        Ok(AboutToReduce(p::RuleID::T0, [T(Some(p::TokenID::NUM))]))
    );

    assert_matches!(
        parser.next_event(),
        Ok(AboutToReduce(p::RuleID::E1, [N(p::SymbolID::T)]))
    );

    assert_matches!(
        parser.next_event(),
        Ok(AboutToReduce(
            p::RuleID::A0,
            [
                N(p::SymbolID::E),
                T(Some(p::TokenID::EQUAL)),
                N(p::SymbolID::E)
            ]
        ))
    );

    assert_matches!(parser.next_event(), Ok(AboutToAccept(N(p::SymbolID::A))));

    assert_matches!(parser.next_event(), Ok(Accepted));
}

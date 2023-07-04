mod p {
    include!(concat!(env!("OUT_DIR"), "/arithmetic_prec.rs"));
}
pub use p::*;

#[allow(unused_imports)]
use lelele_runtime::engine::{ParseEngine, ParseEvent::*, Symbol::*};

#[allow(unused_macros)]
macro_rules! assert_matches {
    ($($t:tt)*) => { assert!(matches!($($t)*)) };
}

#[test]
fn simple_expr() {
    let mut e = ParseEngine::new(ParserDef::default());

    macro_rules! offer_input {
        ($tok:path) => {
            match e.resume() {
                Ok(InputNeeded) => {
                    e.offer_token($tok).unwrap();
                }
                _ => panic!("mismatched"),
            }
        };
        () => {
            match e.resume() {
                Ok(InputNeeded) => {
                    e.offer_eoi().unwrap();
                }
                _ => panic!("mismatched"),
            }
        };
    }

    // tokens: NUM PLUS NUM STAR NUM $eoi

    offer_input!(TokenID::NUM);
    assert_matches!(e.resume(), Ok(Shifting(&TokenID::NUM)));

    offer_input!(TokenID::PLUS);
    assert_matches!(
        e.resume(),
        Ok(AboutToReduce(Symbol::Expr, [T(TokenID::NUM)]))
    );
    assert_matches!(e.resume(), Ok(Shifting(&TokenID::PLUS)));

    offer_input!(TokenID::NUM);
    assert_matches!(e.resume(), Ok(Shifting(&TokenID::NUM)));

    offer_input!(TokenID::STAR);
    assert_matches!(
        e.resume(),
        Ok(AboutToReduce(Symbol::Expr, [T(TokenID::NUM)]))
    );
    assert_matches!(e.resume(), Ok(Shifting(&TokenID::STAR)));

    offer_input!(TokenID::NUM);
    assert_matches!(e.resume(), Ok(Shifting(&TokenID::NUM)));

    offer_input!();
    assert_matches!(
        e.resume(),
        Ok(AboutToReduce(Symbol::Expr, [T(TokenID::NUM)]))
    );
    assert_matches!(
        e.resume(),
        Ok(AboutToReduce(
            Symbol::Expr,
            [N(Symbol::Expr), T(TokenID::STAR), N(Symbol::Expr)]
        ))
    );
    assert_matches!(
        e.resume(),
        Ok(AboutToReduce(
            Symbol::Expr,
            [N(Symbol::Expr), T(TokenID::PLUS), N(Symbol::Expr)]
        ))
    );
    assert_matches!(e.resume(), Ok(Accepted));
}

#[test]
fn with_unary_minus() {
    let mut e = ParseEngine::new(ParserDef::default());

    macro_rules! offer_input {
        ($tok:path) => {
            match e.resume() {
                Ok(InputNeeded) => {
                    e.offer_token($tok).unwrap();
                }
                _ => panic!("mismatched"),
            }
        };
        () => {
            match e.resume() {
                Ok(InputNeeded) => {
                    e.offer_eoi().unwrap();
                }
                _ => panic!("mismatched"),
            }
        };
    }

    // tokens: NUM PLUS NUM STAR MINUS NUM $eoi

    offer_input!(TokenID::NUM);
    assert_matches!(e.resume(), Ok(Shifting(&TokenID::NUM)));

    offer_input!(TokenID::PLUS);
    assert_matches!(
        e.resume(),
        Ok(AboutToReduce(Symbol::Expr, [T(TokenID::NUM)]))
    );
    assert_matches!(e.resume(), Ok(Shifting(&TokenID::PLUS)));

    offer_input!(TokenID::NUM);
    assert_matches!(e.resume(), Ok(Shifting(&TokenID::NUM)));

    offer_input!(TokenID::STAR);
    assert_matches!(
        e.resume(),
        Ok(AboutToReduce(Symbol::Expr, [T(TokenID::NUM)]))
    );
    assert_matches!(e.resume(), Ok(Shifting(&TokenID::STAR)));

    offer_input!(TokenID::MINUS);
    assert_matches!(e.resume(), Ok(Shifting(&TokenID::MINUS)));

    offer_input!(TokenID::NUM);
    assert_matches!(e.resume(), Ok(Shifting(&TokenID::NUM)));

    offer_input!();

    assert_matches!(
        e.resume(),
        Ok(AboutToReduce(Symbol::Expr, [T(TokenID::NUM)]))
    );
    assert_matches!(
        e.resume(),
        Ok(AboutToReduce(
            Symbol::Expr,
            [T(TokenID::MINUS), N(Symbol::Expr)]
        ))
    );
    assert_matches!(
        e.resume(),
        Ok(AboutToReduce(
            Symbol::Expr,
            [N(Symbol::Expr), T(TokenID::STAR), N(Symbol::Expr)]
        ))
    );
    assert_matches!(
        e.resume(),
        Ok(AboutToReduce(
            Symbol::Expr,
            [N(Symbol::Expr), T(TokenID::PLUS), N(Symbol::Expr)]
        ))
    );

    assert_matches!(e.resume(), Ok(Accepted));
}

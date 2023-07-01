mod p {
    include!(concat!(env!("OUT_DIR"), "/g_arithmetic/parser.rs"));
}
pub use p::*;

#[allow(unused_imports)]
use lelele_runtime::parser::{ParseEvent::*, ParseItem::*, Parser};

#[allow(unused_macros)]
macro_rules! assert_matches {
    ($($t:tt)*) => { assert!(matches!($($t)*)) };
}

#[test]
fn simple_expr() {
    let mut parser = Parser::new(ParserDef::default());

    macro_rules! offer_input {
        ($tok:path) => {
            match parser.resume() {
                Ok(InputNeeded) => {
                    parser.offer_token($tok).unwrap();
                }
                _ => panic!("mismatched"),
            }
        };
        () => {
            match parser.resume() {
                Ok(InputNeeded) => {
                    parser.offer_eoi().unwrap();
                }
                _ => panic!("mismatched"),
            }
        };
    }

    // tokens: NUM PLUS NUM STAR NUM $eoi

    offer_input!(TokenID::NUM);
    assert_matches!(parser.resume(), Ok(Shifting(&TokenID::NUM)));

    offer_input!(TokenID::PLUS);
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(Symbol::Atom, [T(TokenID::NUM)]))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(Symbol::Factor, [N(Symbol::Atom)]))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(Symbol::Term, [N(Symbol::Factor)]))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(Symbol::Expr, [N(Symbol::Term)]))
    );
    assert_matches!(parser.resume(), Ok(Shifting(&TokenID::PLUS)));

    offer_input!(TokenID::NUM);
    assert_matches!(parser.resume(), Ok(Shifting(&TokenID::NUM)));

    offer_input!(TokenID::STAR);
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(Symbol::Atom, [T(TokenID::NUM)]))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(Symbol::Factor, [N(Symbol::Atom)]))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(Symbol::Term, [N(Symbol::Factor)]))
    );
    assert_matches!(parser.resume(), Ok(Shifting(&TokenID::STAR)));

    offer_input!(TokenID::NUM);
    assert_matches!(parser.resume(), Ok(Shifting(&TokenID::NUM)));

    offer_input!();
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(Symbol::Atom, [T(TokenID::NUM)]))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(Symbol::Factor, [N(Symbol::Atom)]))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(
            Symbol::Term,
            [N(Symbol::Term), T(TokenID::STAR), N(Symbol::Factor)]
        ))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(
            Symbol::Expr,
            [N(Symbol::Expr), T(TokenID::PLUS), N(Symbol::Term)]
        ))
    );

    assert_matches!(parser.resume(), Ok(Accepted));
}

#[test]
fn with_unary_minus() {
    let mut parser = Parser::new(ParserDef::default());

    macro_rules! offer_input {
        ($tok:path) => {
            match parser.resume() {
                Ok(InputNeeded) => {
                    parser.offer_token($tok).unwrap();
                }
                _ => panic!("mismatched"),
            }
        };
        () => {
            match parser.resume() {
                Ok(InputNeeded) => {
                    parser.offer_eoi().unwrap();
                }
                _ => panic!("mismatched"),
            }
        };
    }

    // tokens: NUM PLUS NUM STAR MINUS NUM $eoi

    offer_input!(TokenID::NUM);
    assert_matches!(parser.resume(), Ok(Shifting(&TokenID::NUM)));

    offer_input!(TokenID::PLUS);
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(Symbol::Atom, [T(TokenID::NUM)]))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(Symbol::Factor, [N(Symbol::Atom)]))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(Symbol::Term, [N(Symbol::Factor)]))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(Symbol::Expr, [N(Symbol::Term)]))
    );
    assert_matches!(parser.resume(), Ok(Shifting(&TokenID::PLUS)));

    offer_input!(TokenID::NUM);
    assert_matches!(parser.resume(), Ok(Shifting(&TokenID::NUM)));

    offer_input!(TokenID::STAR);
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(Symbol::Atom, [T(TokenID::NUM)]))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(Symbol::Factor, [N(Symbol::Atom)]))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(Symbol::Term, [N(Symbol::Factor)]))
    );
    assert_matches!(parser.resume(), Ok(Shifting(&TokenID::STAR)));

    offer_input!(TokenID::MINUS);
    assert_matches!(parser.resume(), Ok(Shifting(&TokenID::MINUS)));

    offer_input!(TokenID::NUM);
    assert_matches!(parser.resume(), Ok(Shifting(&TokenID::NUM)));

    offer_input!();
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(Symbol::Atom, [T(TokenID::NUM)]))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(Symbol::Factor, [N(Symbol::Atom)]))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(
            Symbol::Factor,
            [T(TokenID::MINUS), N(Symbol::Factor)]
        ))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(
            Symbol::Term,
            [N(Symbol::Term), T(TokenID::STAR), N(Symbol::Factor)]
        ))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(
            Symbol::Expr,
            [N(Symbol::Expr), T(TokenID::PLUS), N(Symbol::Term)]
        ))
    );

    assert_matches!(parser.resume(), Ok(Accepted));
}

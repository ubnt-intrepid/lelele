pub mod p {
    include!(concat!(env!("OUT_DIR"), "/g_arithmetic_prec/parser.rs"));
}

#[allow(unused_macros)]
macro_rules! assert_matches {
    ($($t:tt)*) => { assert!(matches!($($t)*)) };
}

#[test]
fn simple_expr() {
    use lelele_runtime::parser::{ParseEvent::*, ParseItem::*};
    use p::{SymbolID, TokenID};

    let mut parser = p::parser();

    macro_rules! offer_input {
        ($tok:path) => {
            match parser.resume() {
                Ok(InputNeeded(sink)) => {
                    sink.offer_token($tok);
                }
                _ => panic!("mismatched"),
            }
        };
        () => {
            match parser.resume() {
                Ok(InputNeeded(sink)) => {
                    sink.offer_eoi();
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
        Ok(AboutToReduce(SymbolID::EXPR, [T(TokenID::NUM)]))
    );
    assert_matches!(parser.resume(), Ok(Shifting(&TokenID::PLUS)));

    offer_input!(TokenID::NUM);
    assert_matches!(parser.resume(), Ok(Shifting(&TokenID::NUM)));

    offer_input!(TokenID::STAR);
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(SymbolID::EXPR, [T(TokenID::NUM)]))
    );
    assert_matches!(parser.resume(), Ok(Shifting(&TokenID::STAR)));

    offer_input!(TokenID::NUM);
    assert_matches!(parser.resume(), Ok(Shifting(&TokenID::NUM)));

    offer_input!();
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(SymbolID::EXPR, [T(TokenID::NUM)]))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(
            SymbolID::EXPR,
            [N(SymbolID::EXPR), T(TokenID::STAR), N(SymbolID::EXPR)]
        ))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(
            SymbolID::EXPR,
            [N(SymbolID::EXPR), T(TokenID::PLUS), N(SymbolID::EXPR)]
        ))
    );
    assert_matches!(parser.resume(), Ok(AboutToAccept(N(SymbolID::EXPR))));
    assert_matches!(parser.resume(), Ok(Accepted));
}

#[test]
fn with_unary_minus() {
    use lelele_runtime::parser::{ParseEvent::*, ParseItem::*};
    use p::{SymbolID, TokenID};

    let mut parser = p::parser();

    macro_rules! offer_input {
        ($tok:path) => {
            match parser.resume() {
                Ok(InputNeeded(sink)) => {
                    sink.offer_token($tok);
                }
                _ => panic!("mismatched"),
            }
        };
        () => {
            match parser.resume() {
                Ok(InputNeeded(sink)) => {
                    sink.offer_eoi();
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
        Ok(AboutToReduce(SymbolID::EXPR, [T(TokenID::NUM)]))
    );
    assert_matches!(parser.resume(), Ok(Shifting(&TokenID::PLUS)));

    offer_input!(TokenID::NUM);
    assert_matches!(parser.resume(), Ok(Shifting(&TokenID::NUM)));

    offer_input!(TokenID::STAR);
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(SymbolID::EXPR, [T(TokenID::NUM)]))
    );
    assert_matches!(parser.resume(), Ok(Shifting(&TokenID::STAR)));

    offer_input!(TokenID::MINUS);
    assert_matches!(parser.resume(), Ok(Shifting(&TokenID::MINUS)));

    offer_input!(TokenID::NUM);
    assert_matches!(parser.resume(), Ok(Shifting(&TokenID::NUM)));

    offer_input!();

    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(SymbolID::EXPR, [T(TokenID::NUM)]))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(
            SymbolID::EXPR,
            [T(TokenID::MINUS), N(SymbolID::EXPR)]
        ))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(
            SymbolID::EXPR,
            [N(SymbolID::EXPR), T(TokenID::STAR), N(SymbolID::EXPR)]
        ))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(
            SymbolID::EXPR,
            [N(SymbolID::EXPR), T(TokenID::PLUS), N(SymbolID::EXPR)]
        ))
    );

    assert_matches!(parser.resume(), Ok(AboutToAccept(N(SymbolID::EXPR))));
    assert_matches!(parser.resume(), Ok(Accepted));
}

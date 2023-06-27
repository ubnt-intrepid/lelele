pub mod p {
    include!(concat!(env!("OUT_DIR"), "/g_arithmetic/parser.rs"));
}

#[allow(unused_macros)]
macro_rules! assert_matches {
    ($($t:tt)*) => { assert!(matches!($($t)*)) };
}

#[test]
fn simple_expr() {
    use lelele_runtime::parser::{ParseEvent::*, ParseItem::*};
    use p::{RuleID, SymbolID, TokenID};

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
    assert_matches!(parser.resume(), Ok(Shifting(Some(&TokenID::NUM))));

    offer_input!(TokenID::PLUS);
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(RuleID::ATOM_NUM, [T(Some(TokenID::NUM))]))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(RuleID::FACTOR_ATOM, [N(SymbolID::ATOM)]))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(RuleID::TERM_FACTOR, [N(SymbolID::FACTOR)]))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(RuleID::EXPR_TERM, [N(SymbolID::TERM)]))
    );
    assert_matches!(parser.resume(), Ok(Shifting(Some(&TokenID::PLUS))));

    offer_input!(TokenID::NUM);
    assert_matches!(parser.resume(), Ok(Shifting(Some(&TokenID::NUM))));

    offer_input!(TokenID::STAR);
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(RuleID::ATOM_NUM, [T(Some(TokenID::NUM))]))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(RuleID::FACTOR_ATOM, [N(SymbolID::ATOM)]))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(RuleID::TERM_FACTOR, [N(SymbolID::FACTOR)]))
    );
    assert_matches!(parser.resume(), Ok(Shifting(Some(&TokenID::STAR))));

    offer_input!(TokenID::NUM);
    assert_matches!(parser.resume(), Ok(Shifting(Some(&TokenID::NUM))));

    offer_input!();
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(RuleID::ATOM_NUM, [T(Some(TokenID::NUM))]))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(RuleID::FACTOR_ATOM, [N(SymbolID::ATOM)]))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(
            RuleID::TERM_MUL,
            [
                N(SymbolID::TERM),
                T(Some(TokenID::STAR)),
                N(SymbolID::FACTOR)
            ]
        ))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(
            RuleID::EXPR_ADD,
            [N(SymbolID::EXPR), T(Some(TokenID::PLUS)), N(SymbolID::TERM)]
        ))
    );

    assert_matches!(parser.resume(), Ok(AboutToAccept(N(SymbolID::EXPR))));
    assert_matches!(parser.resume(), Ok(Accepted));
}

#[test]
fn with_unary_minus() {
    use lelele_runtime::parser::{ParseEvent::*, ParseItem::*};
    use p::{RuleID, SymbolID, TokenID};

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
    assert_matches!(parser.resume(), Ok(Shifting(Some(&TokenID::NUM))));

    offer_input!(TokenID::PLUS);
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(RuleID::ATOM_NUM, [T(Some(TokenID::NUM))]))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(RuleID::FACTOR_ATOM, [N(SymbolID::ATOM)]))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(RuleID::TERM_FACTOR, [N(SymbolID::FACTOR)]))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(RuleID::EXPR_TERM, [N(SymbolID::TERM)]))
    );
    assert_matches!(parser.resume(), Ok(Shifting(Some(&TokenID::PLUS))));

    offer_input!(TokenID::NUM);
    assert_matches!(parser.resume(), Ok(Shifting(Some(&TokenID::NUM))));

    offer_input!(TokenID::STAR);
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(RuleID::ATOM_NUM, [T(Some(TokenID::NUM))]))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(RuleID::FACTOR_ATOM, [N(SymbolID::ATOM)]))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(RuleID::TERM_FACTOR, [N(SymbolID::FACTOR)]))
    );
    assert_matches!(parser.resume(), Ok(Shifting(Some(&TokenID::STAR))));

    offer_input!(TokenID::MINUS);
    assert_matches!(parser.resume(), Ok(Shifting(Some(&TokenID::MINUS))));

    offer_input!(TokenID::NUM);
    assert_matches!(parser.resume(), Ok(Shifting(Some(&TokenID::NUM))));

    offer_input!();
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(RuleID::ATOM_NUM, [T(Some(TokenID::NUM))]))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(RuleID::FACTOR_ATOM, [N(SymbolID::ATOM)]))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(
            RuleID::FACTOR_NEG,
            [T(Some(TokenID::MINUS)), N(SymbolID::FACTOR)]
        ))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(
            RuleID::TERM_MUL,
            [
                N(SymbolID::TERM),
                T(Some(TokenID::STAR)),
                N(SymbolID::FACTOR)
            ]
        ))
    );
    assert_matches!(
        parser.resume(),
        Ok(AboutToReduce(
            RuleID::EXPR_ADD,
            [N(SymbolID::EXPR), T(Some(TokenID::PLUS)), N(SymbolID::TERM)]
        ))
    );

    assert_matches!(parser.resume(), Ok(AboutToAccept(N(SymbolID::EXPR))));
    assert_matches!(parser.resume(), Ok(Accepted));
}

//! Grammar definition for integration tests.

use lelele::grammar::{Assoc, GrammarDef, Precedence};

type Result<T = (), E = lelele::grammar::GrammarDefError> = std::result::Result<T, E>;

pub fn g_simple1(g: &mut GrammarDef<'_>) -> Result {
    let equal = g.token("EQUAL")?;
    let plus = g.token("PLUS")?;
    let ident = g.token("ID")?;
    let num = g.token("NUM")?;

    let a = g.symbol("A")?;
    let e = g.symbol("E")?;
    let t = g.symbol("T")?;

    g.start_symbol(a)?;

    g.rule("A0", a, [e, equal, e])?;
    g.rule("A1", a, [ident])?;

    g.rule("E0", e, [e, plus, t])?;
    g.rule("E1", e, [t])?;

    g.rule("T0", t, [num])?;
    g.rule("T1", t, [ident])?;

    Ok(())
}

pub fn g_simple2(g: &mut GrammarDef<'_>) -> Result {
    // declare terminal symbols.
    let lparen = g.token("LPAREN")?;
    let rparen = g.token("RPAREN")?;
    let plus = g.token("PLUS")?;
    let minus = g.token("MINUS")?;
    let star = g.token("STAR")?;
    let slash = g.token("SLASH")?;
    let num = g.token("NUM")?;

    // declare nonterminal symbols.
    let expr = g.symbol("EXPR")?;
    let term = g.symbol("TERM")?;
    let factor = g.symbol("FACTOR")?;

    g.rule("EXPR1", expr, [expr, plus, term])?; // expr '+' factor
    g.rule("EXPR2", expr, [expr, minus, term])?; // expr '-' factor
    g.rule("EXPR3", expr, [term])?; // factor

    g.rule("TERM1", term, [term, star, factor])?; // factor '*' term
    g.rule("TERM2", term, [term, slash, factor])?; // factor '/' term
    g.rule("TERM3", term, [factor])?; // term

    g.rule("FACTOR1", factor, [num])?; // num
    g.rule("FACTOR2", factor, [lparen, expr, rparen])?; // '(' expr ')'

    g.start_symbol(expr)?;

    Ok(())
}

pub fn g1(g: &mut GrammarDef<'_>) -> Result {
    let plus = g.token("PLUS")?;
    let star = g.token("STAR")?;
    let a = g.token("A")?;

    let e = g.symbol("E")?;
    let t = g.symbol("T")?;

    g.rule("E1", e, [e, plus, t])?;
    g.rule("E2", e, [t])?;

    g.rule("T1", t, [t, star, a])?;
    g.rule("T2", t, [a])?;

    g.start_symbol(e)?;

    Ok(())
}

pub fn g2(g: &mut GrammarDef<'_>) -> Result {
    let comma = g.token("COMMA")?;
    let colon = g.token("COLON")?;
    let ident = g.token("ID")?;

    let def = g.symbol("DEF")?;
    let param_spec = g.symbol("PARAM_SPEC")?;
    let return_spec = g.symbol("RETURN_SPEC")?;
    let type_ = g.symbol("TYPE")?;
    let name = g.symbol("NAME")?;
    let name_list = g.symbol("NAME_LIST")?;

    g.rule("DEF", def, [param_spec, return_spec, comma])?;

    g.rule("PARAM_SPEC_TYPE", param_spec, [type_])?;
    g.rule("PARAM_SPEC_LIST", param_spec, [name_list, colon, type_])?;

    g.rule("RETURN_SPEC_TYPE", return_spec, [type_])?;
    g.rule("RETURN_SPEC_LIST", return_spec, [name, colon, type_])?;

    g.rule("TYPE_IDENT", type_, [ident])?;

    g.rule("NAME_IDENT", name, [ident])?;

    g.rule("NAME_LIST_NAME", name_list, [name])?;
    g.rule("NAME_LIST_LIST", name_list, [name, comma, name_list])?;

    Ok(())
}

// g3 is same as g2

pub fn g4(g: &mut GrammarDef<'_>) -> Result {
    let plus = g.token("PLUS")?;
    let lparen = g.token("LPAREN")?;
    let rparen = g.token("RPAREN")?;
    let num = g.token("NUM")?;

    let e = g.symbol("E")?;
    let t = g.symbol("T")?;

    g.rule("E1", e, [e, plus, t])?;
    g.rule("E2", e, [t])?;

    g.rule("T1", t, [lparen, e, rparen])?;
    g.rule("T2", t, [num])?;

    Ok(())
}

pub fn g5(g: &mut GrammarDef<'_>) -> Result {
    let a = g.token("A")?;
    let plus = g.token("PLUS")?;
    let star = g.token("STAR")?;
    let lparen = g.token("LPAREN")?;
    let rparen = g.token("RPAREN")?;

    let e = g.symbol("E")?;
    let t = g.symbol("T")?;

    g.rule("E1", e, [e, plus, t])?;
    g.rule("T2", e, [t])?;

    g.rule("T1", t, [t, star, a])?;
    g.rule("T2", t, [a])?;
    g.rule("T3", t, [lparen, e, rparen])?;

    Ok(())
}

pub fn g6(g: &mut GrammarDef<'_>) -> Result {
    let a = g.token("A")?;
    let num = g.token("NUM")?;
    let plus = g.token("PLUS")?;
    let lparen = g.token("LPAREN")?;
    let rparen = g.token("RPAREN")?;

    let e = g.symbol("E")?;
    let t = g.symbol("T")?;
    let n = g.symbol("N")?;

    g.rule("E1", e, [e, plus, t, n])?;
    g.rule("E2", e, [t])?;

    g.rule("T1", t, [a])?;
    g.rule("T2", t, [lparen, e, n, rparen])?;
    g.rule("T3", t, [n, a])?;

    g.rule("N1", n, [])?;
    g.rule("N2", n, [num])?;

    Ok(())
}

pub fn g7(g: &mut GrammarDef<'_>) -> Result {
    let t_d = g.token("T_D")?;
    let t_i = g.token("T_I")?;
    let t_r = g.token("T_R")?;
    let t_c = g.token("T_C")?;
    let t_x = g.token("T_X")?;
    let t_f = g.token("T_F")?;
    let t_n = g.token("T_N")?;
    let t_o = g.token("T_O")?;
    let t_a = g.token("T_A")?;
    let t_e = g.token("T_E")?;

    let s_s = g.symbol("N_S")?;
    let s_a = g.symbol("N_A")?;
    let s_t = g.symbol("N_T")?;
    let s_m = g.symbol("N_M")?;
    let s_y = g.symbol("N_Y")?;
    let s_p = g.symbol("N_P")?;
    let s_b = g.symbol("N_B")?;

    g.rule("S", s_s, [t_d, t_i, s_a])?;

    g.rule("A1", s_a, [s_a, s_t])?;
    g.rule("A2", s_a, [])?;

    g.rule("T1", s_t, [s_m])?;
    g.rule("T2", s_t, [s_y])?;
    g.rule("T3", s_t, [s_p])?;
    g.rule("T4", s_t, [s_b])?;

    g.rule("M1", s_m, [t_r])?;
    g.rule("M2", s_m, [t_c])?;

    g.rule("Y1", s_y, [t_x])?;
    g.rule("Y2", s_y, [t_f])?;

    g.rule("P1", s_p, [t_n])?;
    g.rule("P2", s_p, [t_o])?;

    g.rule("B1", s_b, [t_a])?;
    g.rule("B2", s_b, [t_e])?;

    Ok(())
}

pub fn g8(g: &mut GrammarDef<'_>) -> Result {
    let t_x = g.token("X")?;
    let t_y = g.token("Y")?;
    let t_s = g.token("S")?;
    let t_plus = g.token("PLUS")?;

    let s_a = g.symbol("A")?;
    let s_b = g.symbol("B")?;
    let s_c = g.symbol("C")?;
    let s_d = g.symbol("D")?;
    let s_e = g.symbol("E")?;

    g.rule("A1", s_a, [s_a, s_b])?;
    g.rule("A2", s_a, [s_b])?;

    g.rule("B", s_b, [s_c])?;

    g.rule("C1", s_c, [s_d])?;
    g.rule("C2", s_c, [t_x, t_y])?;

    g.rule("D", s_d, [t_s, s_e])?;

    g.rule("E1", s_e, [s_e, t_plus, t_y])?;
    g.rule("E2", s_e, [t_y])?;
    g.rule("E3", s_e, [])?;

    Ok(())
}

pub fn g9(g: &mut GrammarDef<'_>) -> Result {
    let t_a = g.token("T_A")?;
    let t_plus = g.token("T_PLUS")?;
    let t_num = g.token("T_NUM")?;
    let t_lparen = g.token("T_LPAREN")?;
    let t_rparen = g.token("T_RPAREN")?;

    let n_e = g.symbol("N_E")?;
    let n_t = g.symbol("N_T")?;

    g.rule("E1", n_e, [n_e, t_plus, n_t])?;
    g.rule("E2", n_e, [n_t])?;

    g.rule("T1", n_t, [t_a])?;
    g.rule("T2", n_t, [t_num])?;
    g.rule("T3", n_t, [t_lparen, n_e, t_rparen])?;

    Ok(())
}

pub fn g10(g: &mut GrammarDef<'_>) -> Result {
    let t_a = g.token("T_A")?;
    let t_x = g.token("T_X")?;
    let t_lparen = g.token("T_LPAREN")?;
    let t_rparen = g.token("T_RPAREN")?;

    let n_e = g.symbol("N_E")?;
    let n_t = g.symbol("N_T")?;
    let n_o = g.symbol("N_O")?;

    g.rule("E1", n_e, [n_e, n_o, n_t])?;
    g.rule("E2", n_e, [n_t])?;

    g.rule("T1", n_t, [t_a])?;
    g.rule("T2", n_t, [t_lparen, n_e, t_rparen])?;

    g.rule("O1", n_o, [t_x])?;
    g.rule("O2", n_o, [])?;

    Ok(())
}

// TODO: G11 ~ G17

pub fn min_caml(g: &mut GrammarDef<'_>) -> Result {
    // imported from: https://github.com/esumii/min-caml/blob/master/parser.mly

    let mut prec = {
        let mut next_priority = 0;
        move |assoc| {
            let priority = next_priority;
            next_priority += 1;
            Precedence::new(priority, assoc)
        }
    };
    let prec_in = prec(Assoc::Nonassoc);
    let prec_let = prec(Assoc::Right);
    let prec_semicolon = prec(Assoc::Right);
    let prec_if = prec(Assoc::Right);
    let prec_less_minus = prec(Assoc::Right);
    let prec_tuple = prec(Assoc::Nonassoc);
    let prec_comma = prec(Assoc::Left);
    let prec_cmp = prec(Assoc::Left);
    let prec_add = prec(Assoc::Left);
    let prec_mul = prec(Assoc::Left);
    let prec_neg = prec(Assoc::Right);
    let prec_app = prec(Assoc::Left);
    let prec_dot = prec(Assoc::Left);

    let l_paren = g.token("LPAREN")?;
    let r_paren = g.token("RPAREN")?;
    let t_true = g.token("TRUE")?;
    let t_false = g.token("FALSE")?;
    let integer = g.token("INTEGER")?;
    let float = g.token("FLOAT")?;
    let ident = g.token("IDENT")?;
    let t_not = g.token("NOT")?;
    let plus = g.token_with_prec("PLUS", Some(prec_add))?;
    let plus_dot = g.token_with_prec("PLUS_DOT", Some(prec_add))?;
    let minus = g.token_with_prec("MINUS", Some(prec_add))?;
    let minus_dot = g.token_with_prec("MINUS_DOT", Some(prec_add))?;
    let star_dot = g.token_with_prec("STAR_DOT", Some(prec_mul))?;
    let slash_dot = g.token_with_prec("SLASH_DOT", Some(prec_mul))?;
    let equal = g.token_with_prec("EQUAL", Some(prec_cmp))?;
    let less_greater = g.token_with_prec("LESS_GREATER", Some(prec_cmp))?;
    let less = g.token_with_prec("LESS", Some(prec_cmp))?;
    let greater = g.token_with_prec("GREATER", Some(prec_cmp))?;
    let less_equal = g.token_with_prec("LESS_EQUAL", Some(prec_cmp))?;
    let greater_equal = g.token_with_prec("GREATER_EQUAL", Some(prec_cmp))?;
    let less_minus = g.token_with_prec("LESS_MINUS", Some(prec_less_minus))?;
    let comma = g.token_with_prec("COMMA", Some(prec_comma))?;
    let semicolon = g.token_with_prec("SEMICOLON", Some(prec_semicolon))?;
    let t_if = g.token("IF")?;
    let t_then = g.token("THEN")?;
    let t_else = g.token("ELSE")?;
    let t_let = g.token("LET")?;
    let t_rec = g.token("REC")?;
    let t_in = g.token_with_prec("IN", Some(prec_in))?;
    let array_make = g.token("ARRAY_MAKE")?;
    let dot = g.token_with_prec("DOT", Some(prec_dot))?;

    let simple_exp = g.symbol("SIMPLE_EXP")?;
    let exp = g.symbol("EXP")?;
    let formal_args = g.symbol("FORMAL_ARGS")?;
    let actual_args = g.symbol("ACTUAL_ARGS")?;
    let fundef = g.symbol("FUNDEF")?;
    let pat = g.symbol("PAT")?;
    let elems = g.symbol("ELEMS")?;

    g.start_symbol(exp)?;

    g.rule("SIMPLE_EXP_PAREN", simple_exp, [l_paren, exp, r_paren])?;
    g.rule("SIMPLE_EXP_UNIT", simple_exp, [l_paren, r_paren])?;
    g.rule("SIMPLE_EXP_TRUE", simple_exp, [t_true])?;
    g.rule("SIMPLE_EXP_FALSE", simple_exp, [t_false])?;
    g.rule("SIMPLE_EXP_INT", simple_exp, [integer])?;
    g.rule("SIMPLE_EXP_FLOAT", simple_exp, [float])?;
    g.rule("SIMPLE_EXP_IDENT", simple_exp, [ident])?;
    g.rule(
        "SIMPLE_EXP_ARRAY_GET",
        simple_exp,
        [simple_exp, dot, l_paren, exp, r_paren],
    )?;

    g.rule("EXP_REDIRECT", exp, [simple_exp])?;
    g.rule_with_prec("EXP_NOT", exp, [t_not, exp], Some(prec_app))?;
    g.rule_with_prec("EXP_NEG", exp, [minus, exp], Some(prec_neg))?;
    g.rule_with_prec("EXP_NEG_DOT", exp, [minus_dot, exp], Some(prec_neg))?;
    g.rule("EXP_ADD", exp, [exp, plus, exp])?;
    g.rule("EXP_SUB", exp, [exp, minus, exp])?;
    g.rule("EXP_FADD", exp, [exp, plus_dot, exp])?;
    g.rule("EXP_FSUB", exp, [exp, minus_dot, exp])?;
    g.rule("EXP_FMUL", exp, [exp, star_dot, exp])?;
    g.rule("EXP_FDIV", exp, [exp, slash_dot, exp])?;
    g.rule("EXP_EQUAL", exp, [exp, equal, exp])?;
    g.rule("EXP_LESS_GREATER", exp, [exp, less_greater, exp])?;
    g.rule("EXP_LESS", exp, [exp, less, exp])?;
    g.rule("EXP_GREATER", exp, [exp, greater, exp])?;
    g.rule("EXP_LESS_EQ", exp, [exp, less_equal, exp])?;
    g.rule("EXP_GREATER_EQ", exp, [exp, greater_equal, exp])?;
    g.rule_with_prec(
        "EXP_IF",
        exp,
        [t_if, exp, t_then, exp, t_else, exp],
        Some(prec_if),
    )?;
    g.rule_with_prec(
        "EXP_LET",
        exp,
        [t_let, ident, equal, exp, t_in, exp],
        Some(prec_let),
    )?;
    g.rule_with_prec(
        "EXP_LET_REC",
        exp,
        [t_let, t_rec, fundef, t_in, exp],
        Some(prec_let),
    )?;
    g.rule(
        "EXP_LET_TUPLE",
        exp,
        [t_let, l_paren, pat, r_paren, equal, exp, t_in, exp],
    )?;
    g.rule_with_prec("EXP_APP", exp, [simple_exp, actual_args], Some(prec_app))?;
    g.rule_with_prec("EXP_ELEMS", exp, [elems], Some(prec_tuple))?;
    g.rule(
        "EXP_ARRAY_PUT",
        exp,
        [simple_exp, dot, l_paren, exp, r_paren, less_minus, exp],
    )?;
    g.rule("EXP_SEMICOLON", exp, [exp, semicolon, exp])?;
    g.rule_with_prec(
        "EXP_ARRAY_CREATE",
        exp,
        [array_make, simple_exp, simple_exp],
        Some(prec_app),
    )?;

    g.rule("FUNDEF", fundef, [ident, formal_args, equal, exp])?;

    g.rule("FORMAL_ARGS_LIST", formal_args, [ident, formal_args])?;
    g.rule("FORMAL_ARGS_ELEM", formal_args, [ident])?;

    g.rule_with_prec(
        "ACTUAL_ARGS_LIST",
        actual_args,
        [actual_args, simple_exp],
        Some(prec_app),
    )?;
    g.rule_with_prec(
        "ACTUAL_ARGS_ELEM",
        actual_args,
        [simple_exp],
        Some(prec_app),
    )?;

    g.rule("ELEMS", elems, [elems, comma, exp])?;
    g.rule("ELEMS_TAIL", elems, [exp, comma, exp])?;

    g.rule("PAT_LIST", pat, [pat, comma, ident])?;
    g.rule("PAT_TAIL", pat, [ident, comma, ident])?;

    Ok(())
}

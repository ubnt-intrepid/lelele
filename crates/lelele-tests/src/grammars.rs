//! Grammar definition for integration tests.

use lelele::grammar::GrammarDef;

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

    g.rule(a, [e, equal, e])?;
    g.rule(a, [ident])?;

    g.rule(e, [e, plus, t])?;
    g.rule(e, [t])?;

    g.rule(t, [num])?;
    g.rule(t, [ident])?;

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
    let factor = g.symbol("FACTOR")?;
    let term = g.symbol("TERM")?;

    g.rule(expr, [expr, plus, factor])?; // expr '+' factor
    g.rule(expr, [expr, minus, factor])?; // expr '-' factor
    g.rule(expr, [factor])?; // factor

    g.rule(expr, [factor, star, term])?; // factor '*' term
    g.rule(expr, [factor, slash, term])?; // factor '/' term
    g.rule(expr, [term])?; // term

    g.rule(term, [num])?; // num
    g.rule(term, [lparen, expr, rparen])?; // '(' expr ')'

    g.start_symbol(expr)?;

    Ok(())
}

pub fn g1(g: &mut GrammarDef<'_>) -> Result {
    let plus = g.token("PLUS")?;
    let star = g.token("STAR")?;
    let a = g.token("A")?;

    let e = g.symbol("E")?;
    let t = g.symbol("T")?;

    g.rule(e, [e, plus, t])?;
    g.rule(e, [t])?;

    g.rule(t, [t, star, a])?;
    g.rule(t, [a])?;

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

    g.rule(def, [param_spec, return_spec, comma])?;

    g.rule(param_spec, [type_])?;
    g.rule(param_spec, [name_list, colon, type_])?;

    g.rule(return_spec, [type_])?;
    g.rule(return_spec, [name, colon, type_])?;

    g.rule(type_, [ident])?;

    g.rule(name, [ident])?;

    g.rule(name_list, [name])?;
    g.rule(name_list, [name, comma, name_list])?;

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

    g.rule(e, [e, plus, t])?;
    g.rule(e, [t])?;

    g.rule(t, [lparen, e, rparen])?;
    g.rule(t, [num])?;

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

    g.rule(e, [e, plus, t])?;
    g.rule(e, [t])?;

    g.rule(t, [t, star, a])?;
    g.rule(t, [a])?;
    g.rule(t, [lparen, e, rparen])?;

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

    g.rule(e, [e, plus, t, n])?;
    g.rule(e, [t])?;

    g.rule(t, [a])?;
    g.rule(t, [lparen, e, n, rparen])?;
    g.rule(t, [n, a])?;

    g.rule(n, [])?;
    g.rule(n, [num])?;

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

    g.rule(s_s, [t_d, t_i, s_a])?;

    g.rule(s_a, [s_a, s_t])?;
    g.rule(s_a, [])?;

    g.rule(s_t, [s_m])?;
    g.rule(s_t, [s_y])?;
    g.rule(s_t, [s_p])?;
    g.rule(s_t, [s_b])?;

    g.rule(s_m, [t_r])?;
    g.rule(s_m, [t_c])?;

    g.rule(s_y, [t_x])?;
    g.rule(s_y, [t_f])?;

    g.rule(s_p, [t_n])?;
    g.rule(s_p, [t_o])?;

    g.rule(s_b, [t_a])?;
    g.rule(s_b, [t_e])?;

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

    g.rule(s_a, [s_a, s_b])?;
    g.rule(s_a, [s_b])?;

    g.rule(s_b, [s_c])?;
    g.rule(s_c, [s_d])?;

    g.rule(s_c, [t_x, t_y])?;

    g.rule(s_d, [t_s, s_e])?;

    g.rule(s_e, [s_e, t_plus, t_y])?;
    g.rule(s_e, [t_y])?;
    g.rule(s_e, [])?;

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

    g.rule(n_e, [n_e, t_plus, n_t])?;
    g.rule(n_e, [n_t])?;

    g.rule(n_t, [t_a])?;
    g.rule(n_t, [t_num])?;
    g.rule(n_t, [t_lparen, n_e, t_rparen])?;

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

    g.rule(n_e, [n_e, n_o, n_t])?;
    g.rule(n_e, [n_t])?;

    g.rule(n_t, [t_a])?;
    g.rule(n_t, [t_lparen, n_e, t_rparen])?;

    g.rule(n_o, [t_x])?;
    g.rule(n_o, [])?;

    Ok(())
}

// TODO: G11 ~ G17

pub fn min_caml(g: &mut GrammarDef<'_>) -> Result {
    // WIP

    let l_paren = g.token("LPAREN")?;
    let r_paren = g.token("RPAREN")?;
    let t_true = g.token("TRUE")?;
    let t_false = g.token("FALSE")?;
    let integer = g.token("INTEGER")?;
    let float = g.token("FLOAT")?;
    let ident = g.token("IDENT")?;
    let t_not = g.token("NOT")?;
    let plus = g.token("PLUS")?;
    let plus_dot = g.token("PLUS_DOT")?;
    let minus = g.token("MINUS")?;
    let minus_dot = g.token("MINUS_DOT")?;
    let star_dot = g.token("STAR_DOT")?;
    let slash_dot = g.token("SLASH_DOT")?;
    let equal = g.token("EQUAL")?;
    let less_greater = g.token("LESS_GREATER")?;
    let less = g.token("LESS")?;
    let greater = g.token("GREATER")?;
    let less_equal = g.token("LESS_EQUAL")?;
    let greater_equal = g.token("GREATER_EQUAL")?;
    let less_minus = g.token("LESS_MINUS")?;
    let comma = g.token("COMMA")?;
    let semicolon = g.token("SEMICOLON")?;
    let t_if = g.token("IF")?;
    let t_then = g.token("THEN")?;
    let t_else = g.token("ELSE")?;
    let t_let = g.token("LET")?;
    let t_rec = g.token("REC")?;
    let t_in = g.token("IN")?;
    let array_make = g.token("ARRAY_MAKE")?;
    let dot = g.token("DOT")?;

    let simple_exp = g.symbol("SIMPLE_EXP")?;
    let app_exp = g.symbol("APP_EXP")?;
    let neg_exp = g.symbol("NEG_EXP")?;
    let mult_exp = g.symbol("MULT_EXP")?;
    let add_exp = g.symbol("ADD_EXP")?;
    let rel_exp = g.symbol("REL_EXP")?;
    let tuple_exp = g.symbol("TUPLE_EXP")?;
    let put_exp = g.symbol("PUT_EXP")?;
    let if_exp = g.symbol("IF_EXP")?;
    let let_exp = g.symbol("LET_EXP")?;
    let exp = g.symbol("EXPR")?;
    let formal_args = g.symbol("FORMAL_ARGS")?;
    let actual_args = g.symbol("ACTUAL_ARGS")?;
    let tuple_exp_rest = g.symbol("TUPLE_EXP_REST")?;
    let fundef = g.symbol("FUNDEF")?;
    let pat = g.symbol("PAT")?;

    g.rule(simple_exp, [l_paren, exp, r_paren])?;
    g.rule(simple_exp, [l_paren, r_paren])?;
    g.rule(simple_exp, [t_true])?;
    g.rule(simple_exp, [t_false])?;
    g.rule(simple_exp, [integer])?;
    g.rule(simple_exp, [float])?;
    g.rule(simple_exp, [ident])?;
    g.rule(simple_exp, [simple_exp, dot, l_paren, exp, r_paren])?;

    g.rule(app_exp, [simple_exp])?;
    g.rule(app_exp, [simple_exp, actual_args])?;
    g.rule(app_exp, [array_make, simple_exp, simple_exp])?;
    g.rule(app_exp, [t_not, app_exp])?;

    g.rule(neg_exp, [app_exp])?;
    g.rule(neg_exp, [minus, neg_exp])?;
    g.rule(neg_exp, [minus_dot, neg_exp])?;

    g.rule(mult_exp, [neg_exp])?;
    g.rule(mult_exp, [mult_exp, star_dot, neg_exp])?;
    g.rule(mult_exp, [mult_exp, slash_dot, neg_exp])?;

    g.rule(add_exp, [mult_exp])?;
    g.rule(add_exp, [add_exp, plus, mult_exp])?;
    g.rule(add_exp, [add_exp, minus, mult_exp])?;
    g.rule(add_exp, [add_exp, plus_dot, mult_exp])?;
    g.rule(add_exp, [add_exp, minus_dot, mult_exp])?;

    g.rule(rel_exp, [add_exp])?;
    g.rule(rel_exp, [rel_exp, equal, add_exp])?;
    g.rule(rel_exp, [rel_exp, less_greater, add_exp])?;
    g.rule(rel_exp, [rel_exp, less, add_exp])?;
    g.rule(rel_exp, [rel_exp, greater, add_exp])?;
    g.rule(rel_exp, [rel_exp, less_equal, add_exp])?;
    g.rule(rel_exp, [rel_exp, greater_equal, add_exp])?;

    g.rule(tuple_exp, [rel_exp])?;
    g.rule(tuple_exp, [rel_exp, comma, tuple_exp_rest])?;

    g.rule(tuple_exp_rest, [rel_exp])?;
    g.rule(tuple_exp_rest, [rel_exp, comma, tuple_exp_rest])?;

    g.rule(put_exp, [tuple_exp])?;
    g.rule(
        put_exp,
        [simple_exp, dot, l_paren, exp, r_paren, less_minus, exp],
    )?;

    g.rule(if_exp, [put_exp])?;
    g.rule(if_exp, [t_if, exp, t_then, exp, t_else, exp])?;

    g.rule(let_exp, [t_let, ident, equal, exp, t_in, exp])?;
    g.rule(let_exp, [t_let, t_rec, fundef, t_in, exp])?;
    g.rule(
        let_exp,
        [t_let, l_paren, pat, r_paren, equal, exp, t_in, exp],
    )?;

    g.rule(fundef, [ident, formal_args, equal, exp])?;

    g.rule(formal_args, [ident, formal_args])?;
    g.rule(formal_args, [ident])?;

    g.rule(actual_args, [actual_args, simple_exp])?;
    g.rule(actual_args, [simple_exp])?;

    g.rule(pat, [pat, comma, ident])?;
    g.rule(pat, [ident, comma, ident])?;

    g.rule(exp, [if_exp])?;
    g.rule(exp, [if_exp, semicolon, exp])?;
    g.rule(exp, [let_exp])?;

    g.start_symbol(exp)?;

    Ok(())
}

//! Grammar definition for integration tests.

use lelele::grammar::{Assoc, GrammarDef, Precedence};

type Result<T = (), E = lelele::grammar::GrammarDefError> = std::result::Result<T, E>;

pub fn g_simple1(g: &mut GrammarDef<'_>) -> Result {
    let equal = g.terminal("EQUAL", None)?;
    let plus = g.terminal("PLUS", None)?;
    let ident = g.terminal("ID", None)?;
    let num = g.terminal("NUM", None)?;

    let a = g.nonterminal("A")?;
    let e = g.nonterminal("E")?;
    let t = g.nonterminal("T")?;

    g.start_symbol(a)?;

    g.rule(a, [e, equal, e], "A0", None)?;
    g.rule(a, [ident], "A1", None)?;

    g.rule(e, [e, plus, t], "E0", None)?;
    g.rule(e, [t], "E1", None)?;

    g.rule(t, [num], "T0", None)?;
    g.rule(t, [ident], "T1", None)?;

    Ok(())
}

pub fn g_simple2(g: &mut GrammarDef<'_>) -> Result {
    // declare terminal symbols.
    let lparen = g.terminal("LPAREN", None)?;
    let rparen = g.terminal("RPAREN", None)?;
    let plus = g.terminal("PLUS", None)?;
    let minus = g.terminal("MINUS", None)?;
    let star = g.terminal("STAR", None)?;
    let slash = g.terminal("SLASH", None)?;
    let num = g.terminal("NUM", None)?;

    // declare nonterminal symbols.
    let expr = g.nonterminal("EXPR")?;
    let term = g.nonterminal("TERM")?;
    let factor = g.nonterminal("FACTOR")?;

    g.rule(expr, [expr, plus, term], "EXPR1", None)?;
    g.rule(expr, [expr, minus, term], "EXPR2", None)?;
    g.rule(expr, [term], "EXPR3", None)?;

    g.rule(term, [term, star, factor], "TERM1", None)?;
    g.rule(term, [term, slash, factor], "TERM2", None)?;
    g.rule(term, [factor], "TERM3", None)?;

    g.rule(factor, [num], "FACTOR1", None)?;
    g.rule(factor, [lparen, expr, rparen], "FACTOR2", None)?;

    g.start_symbol(expr)?;

    Ok(())
}

pub fn g1(g: &mut GrammarDef<'_>) -> Result {
    let plus = g.terminal("PLUS", None)?;
    let star = g.terminal("STAR", None)?;
    let a = g.terminal("A", None)?;

    let e = g.nonterminal("E")?;
    let t = g.nonterminal("T")?;

    g.rule(e, [e, plus, t], "E1", None)?;
    g.rule(e, [t], "E2", None)?;

    g.rule(t, [t, star, a], "T1", None)?;
    g.rule(t, [a], "T2", None)?;

    g.start_symbol(e)?;

    Ok(())
}

pub fn g2(g: &mut GrammarDef<'_>) -> Result {
    let comma = g.terminal("COMMA", None)?;
    let colon = g.terminal("COLON", None)?;
    let ident = g.terminal("ID", None)?;

    let def = g.nonterminal("DEF")?;
    let param_spec = g.nonterminal("PARAM_SPEC")?;
    let return_spec = g.nonterminal("RETURN_SPEC")?;
    let type_ = g.nonterminal("TYPE")?;
    let name = g.nonterminal("NAME")?;
    let name_list = g.nonterminal("NAME_LIST")?;

    g.rule(def, [param_spec, return_spec, comma], "DEF", None)?;

    g.rule(param_spec, [type_], "PARAM_SPEC_TYPE", None)?;
    g.rule(
        param_spec,
        [name_list, colon, type_],
        "PARAM_SPEC_LIST",
        None,
    )?;

    g.rule(return_spec, [type_], "RETURN_SPEC_TYPE", None)?;
    g.rule(return_spec, [name, colon, type_], "RETURN_SPEC_LIST", None)?;

    g.rule(type_, [ident], "TYPE_IDENT", None)?;

    g.rule(name, [ident], "NAME_IDENT", None)?;

    g.rule(name_list, [name], "NAME_LIST_NAME", None)?;
    g.rule(name_list, [name, comma, name_list], "NAME_LIST_LIST", None)?;

    Ok(())
}

// g3 is same as g2

pub fn g4(g: &mut GrammarDef<'_>) -> Result {
    let plus = g.terminal("PLUS", None)?;
    let lparen = g.terminal("LPAREN", None)?;
    let rparen = g.terminal("RPAREN", None)?;
    let num = g.terminal("NUM", None)?;

    let e = g.nonterminal("E")?;
    let t = g.nonterminal("T")?;

    g.rule(e, [e, plus, t], "E1", None)?;
    g.rule(e, [t], "E2", None)?;

    g.rule(t, [lparen, e, rparen], "T1", None)?;
    g.rule(t, [num], "T2", None)?;

    Ok(())
}

pub fn g5(g: &mut GrammarDef<'_>) -> Result {
    let a = g.terminal("A", None)?;
    let plus = g.terminal("PLUS", None)?;
    let star = g.terminal("STAR", None)?;
    let lparen = g.terminal("LPAREN", None)?;
    let rparen = g.terminal("RPAREN", None)?;

    let e = g.nonterminal("E")?;
    let t = g.nonterminal("T")?;

    g.rule(e, [e, plus, t], "E1", None)?;
    g.rule(e, [t], "T2", None)?;

    g.rule(t, [t, star, a], "T1", None)?;
    g.rule(t, [a], "T2", None)?;
    g.rule(t, [lparen, e, rparen], "T3", None)?;

    Ok(())
}

pub fn g6(g: &mut GrammarDef<'_>) -> Result {
    let a = g.terminal("A", None)?;
    let num = g.terminal("NUM", None)?;
    let plus = g.terminal("PLUS", None)?;
    let lparen = g.terminal("LPAREN", None)?;
    let rparen = g.terminal("RPAREN", None)?;

    let e = g.nonterminal("E")?;
    let t = g.nonterminal("T")?;
    let n = g.nonterminal("N")?;

    g.rule(e, [e, plus, t, n], "E1", None)?;
    g.rule(e, [t], "E2", None)?;

    g.rule(t, [a], "T1", None)?;
    g.rule(t, [lparen, e, n, rparen], "T2", None)?;
    g.rule(t, [n, a], "T3", None)?;

    g.rule(n, [], "N1", None)?;
    g.rule(n, [num], "N2", None)?;

    Ok(())
}

pub fn g7(g: &mut GrammarDef<'_>) -> Result {
    let t_d = g.terminal("D", None)?;
    let t_i = g.terminal("I", None)?;
    let t_r = g.terminal("R", None)?;
    let t_c = g.terminal("C", None)?;
    let t_x = g.terminal("X", None)?;
    let t_f = g.terminal("F", None)?;
    let t_n = g.terminal("N", None)?;
    let t_o = g.terminal("O", None)?;
    let t_a = g.terminal("A", None)?;
    let t_e = g.terminal("E", None)?;

    let s_s = g.nonterminal("S")?;
    let s_a = g.nonterminal("A")?;
    let s_t = g.nonterminal("T")?;
    let s_m = g.nonterminal("M")?;
    let s_y = g.nonterminal("Y")?;
    let s_p = g.nonterminal("P")?;
    let s_b = g.nonterminal("B")?;

    g.rule(s_s, [t_d, t_i, s_a], "S", None)?;

    g.rule(s_a, [s_a, s_t], "A1", None)?;
    g.rule(s_a, [], "A2", None)?;

    g.rule(s_t, [s_m], "T1", None)?;
    g.rule(s_t, [s_y], "T2", None)?;
    g.rule(s_t, [s_p], "T3", None)?;
    g.rule(s_t, [s_b], "T4", None)?;

    g.rule(s_m, [t_r], "M1", None)?;
    g.rule(s_m, [t_c], "M2", None)?;

    g.rule(s_y, [t_x], "Y1", None)?;
    g.rule(s_y, [t_f], "Y2", None)?;

    g.rule(s_p, [t_n], "P1", None)?;
    g.rule(s_p, [t_o], "P2", None)?;

    g.rule(s_b, [t_a], "B1", None)?;
    g.rule(s_b, [t_e], "B2", None)?;

    Ok(())
}

pub fn g8(g: &mut GrammarDef<'_>) -> Result {
    let t_x = g.terminal("X", None)?;
    let t_y = g.terminal("Y", None)?;
    let t_s = g.terminal("S", None)?;
    let t_plus = g.terminal("PLUS", None)?;

    let s_a = g.nonterminal("A")?;
    let s_b = g.nonterminal("B")?;
    let s_c = g.nonterminal("C")?;
    let s_d = g.nonterminal("D")?;
    let s_e = g.nonterminal("E")?;

    g.rule(s_a, [s_a, s_b], "A1", None)?;
    g.rule(s_a, [s_b], "A2", None)?;

    g.rule(s_b, [s_c], "B", None)?;

    g.rule(s_c, [s_d], "C1", None)?;
    g.rule(s_c, [t_x, t_y], "C2", None)?;

    g.rule(s_d, [t_s, s_e], "D", None)?;

    g.rule(s_e, [s_e, t_plus, t_y], "E1", None)?;
    g.rule(s_e, [t_y], "E2", None)?;
    g.rule(s_e, [], "E3", None)?;

    Ok(())
}

pub fn g9(g: &mut GrammarDef<'_>) -> Result {
    let t_a = g.terminal("T_A", None)?;
    let t_plus = g.terminal("T_PLUS", None)?;
    let t_num = g.terminal("T_NUM", None)?;
    let t_lparen = g.terminal("T_LPAREN", None)?;
    let t_rparen = g.terminal("T_RPAREN", None)?;

    let n_e = g.nonterminal("N_E")?;
    let n_t = g.nonterminal("N_T")?;

    g.rule(n_e, [n_e, t_plus, n_t], "E1", None)?;
    g.rule(n_e, [n_t], "E2", None)?;

    g.rule(n_t, [t_a], "T1", None)?;
    g.rule(n_t, [t_num], "T2", None)?;
    g.rule(n_t, [t_lparen, n_e, t_rparen], "T3", None)?;

    Ok(())
}

pub fn g10(g: &mut GrammarDef<'_>) -> Result {
    let t_a = g.terminal("T_A", None)?;
    let t_x = g.terminal("T_X", None)?;
    let t_lparen = g.terminal("T_LPAREN", None)?;
    let t_rparen = g.terminal("T_RPAREN", None)?;

    let n_e = g.nonterminal("N_E")?;
    let n_t = g.nonterminal("N_T")?;
    let n_o = g.nonterminal("N_O")?;

    g.rule(n_e, [n_e, n_o, n_t], "E1", None)?;
    g.rule(n_e, [n_t], "E2", None)?;

    g.rule(n_t, [t_a], "T1", None)?;
    g.rule(n_t, [t_lparen, n_e, t_rparen], "T2", None)?;

    g.rule(n_o, [t_x], "O1", None)?;
    g.rule(n_o, [], "O2", None)?;

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

    let l_paren = g.terminal("LPAREN", None)?;
    let r_paren = g.terminal("RPAREN", None)?;
    let t_true = g.terminal("TRUE", None)?;
    let t_false = g.terminal("FALSE", None)?;
    let integer = g.terminal("INTEGER", None)?;
    let float = g.terminal("FLOAT", None)?;
    let ident = g.terminal("IDENT", None)?;
    let t_not = g.terminal("NOT", None)?;
    let plus = g.terminal("PLUS", Some(prec_add))?;
    let plus_dot = g.terminal("PLUS_DOT", Some(prec_add))?;
    let minus = g.terminal("MINUS", Some(prec_add))?;
    let minus_dot = g.terminal("MINUS_DOT", Some(prec_add))?;
    let star_dot = g.terminal("STAR_DOT", Some(prec_mul))?;
    let slash_dot = g.terminal("SLASH_DOT", Some(prec_mul))?;
    let equal = g.terminal("EQUAL", Some(prec_cmp))?;
    let less_greater = g.terminal("LESS_GREATER", Some(prec_cmp))?;
    let less = g.terminal("LESS", Some(prec_cmp))?;
    let greater = g.terminal("GREATER", Some(prec_cmp))?;
    let less_equal = g.terminal("LESS_EQUAL", Some(prec_cmp))?;
    let greater_equal = g.terminal("GREATER_EQUAL", Some(prec_cmp))?;
    let less_minus = g.terminal("LESS_MINUS", Some(prec_less_minus))?;
    let comma = g.terminal("COMMA", Some(prec_comma))?;
    let semicolon = g.terminal("SEMICOLON", Some(prec_semicolon))?;
    let t_if = g.terminal("IF", None)?;
    let t_then = g.terminal("THEN", None)?;
    let t_else = g.terminal("ELSE", None)?;
    let t_let = g.terminal("LET", None)?;
    let t_rec = g.terminal("REC", None)?;
    let t_in = g.terminal("IN", Some(prec_in))?;
    let array_make = g.terminal("ARRAY_MAKE", None)?;
    let dot = g.terminal("DOT", Some(prec_dot))?;

    let simple_exp = g.nonterminal("SIMPLE_EXP")?;
    let exp = g.nonterminal("EXP")?;
    let formal_args = g.nonterminal("FORMAL_ARGS")?;
    let actual_args = g.nonterminal("ACTUAL_ARGS")?;
    let fundef = g.nonterminal("FUNDEF")?;
    let pat = g.nonterminal("PAT")?;
    let elems = g.nonterminal("ELEMS")?;

    g.start_symbol(exp)?;

    g.rule(
        simple_exp,
        [l_paren, exp, r_paren],
        "SIMPLE_EXP_PAREN",
        None,
    )?;
    g.rule(simple_exp, [l_paren, r_paren], "SIMPLE_EXP_UNIT", None)?;
    g.rule(simple_exp, [t_true], "SIMPLE_EXP_TRUE", None)?;
    g.rule(simple_exp, [t_false], "SIMPLE_EXP_FALSE", None)?;
    g.rule(simple_exp, [integer], "SIMPLE_EXP_INT", None)?;
    g.rule(simple_exp, [float], "SIMPLE_EXP_FLOAT", None)?;
    g.rule(simple_exp, [ident], "SIMPLE_EXP_IDENT", None)?;
    g.rule(
        simple_exp,
        [simple_exp, dot, l_paren, exp, r_paren],
        "SIMPLE_EXP_ARRAY_GET",
        None,
    )?;

    g.rule(exp, [simple_exp], "EXP_REDIRECT", None)?;
    g.rule(exp, [t_not, exp], "EXP_NOT", Some(prec_app))?;
    g.rule(exp, [minus, exp], "EXP_NEG", Some(prec_neg))?;
    g.rule(exp, [minus_dot, exp], "EXP_NEG_DOT", Some(prec_neg))?;
    g.rule(exp, [exp, plus, exp], "EXP_ADD", None)?;
    g.rule(exp, [exp, minus, exp], "EXP_SUB", None)?;
    g.rule(exp, [exp, plus_dot, exp], "EXP_FADD", None)?;
    g.rule(exp, [exp, minus_dot, exp], "EXP_FSUB", None)?;
    g.rule(exp, [exp, star_dot, exp], "EXP_FMUL", None)?;
    g.rule(exp, [exp, slash_dot, exp], "EXP_FDIV", None)?;
    g.rule(exp, [exp, equal, exp], "EXP_EQUAL", None)?;
    g.rule(exp, [exp, less_greater, exp], "EXP_LESS_GREATER", None)?;
    g.rule(exp, [exp, less, exp], "EXP_LESS", None)?;
    g.rule(exp, [exp, greater, exp], "EXP_GREATER", None)?;
    g.rule(exp, [exp, less_equal, exp], "EXP_LESS_EQ", None)?;
    g.rule(exp, [exp, greater_equal, exp], "EXP_GREATER_EQ", None)?;
    g.rule(
        exp,
        [t_if, exp, t_then, exp, t_else, exp],
        "EXP_IF",
        Some(prec_if),
    )?;
    g.rule(
        exp,
        [t_let, ident, equal, exp, t_in, exp],
        "EXP_LET",
        Some(prec_let),
    )?;
    g.rule(
        exp,
        [t_let, t_rec, fundef, t_in, exp],
        "EXP_LET_REC",
        Some(prec_let),
    )?;
    g.rule(
        exp,
        [t_let, l_paren, pat, r_paren, equal, exp, t_in, exp],
        "EXP_LET_TUPLE",
        None,
    )?;
    g.rule(exp, [simple_exp, actual_args], "EXP_APP", Some(prec_app))?;
    g.rule(exp, [elems], "EXP_ELEMS", Some(prec_tuple))?;
    g.rule(
        exp,
        [simple_exp, dot, l_paren, exp, r_paren, less_minus, exp],
        "EXP_ARRAY_PUT",
        None,
    )?;
    g.rule(exp, [exp, semicolon, exp], "EXP_SEMICOLON", None)?;
    g.rule(
        exp,
        [array_make, simple_exp, simple_exp],
        "EXP_ARRAY_CREATE",
        Some(prec_app),
    )?;

    g.rule(fundef, [ident, formal_args, equal, exp], "FUNDEF", None)?;

    g.rule(formal_args, [ident, formal_args], "FORMAL_ARGS_LIST", None)?;
    g.rule(formal_args, [ident], "FORMAL_ARGS_ELEM", None)?;

    g.rule(
        actual_args,
        [actual_args, simple_exp],
        "ACTUAL_ARGS_LIST",
        Some(prec_app),
    )?;
    g.rule(
        actual_args,
        [simple_exp],
        "ACTUAL_ARGS_ELEM",
        Some(prec_app),
    )?;

    g.rule(elems, [elems, comma, exp], "ELEMS", None)?;
    g.rule(elems, [exp, comma, exp], "ELEMS_TAIL", None)?;

    g.rule(pat, [pat, comma, ident], "PAT_LIST", None)?;
    g.rule(pat, [ident, comma, ident], "PAT_TAIL", None)?;

    Ok(())
}

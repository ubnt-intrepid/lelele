//! Grammar definition for integration tests.

use lelele::grammar::{Assoc, GrammarDef, Precedence};

type Result<T = (), E = lelele::grammar::GrammarDefError> = std::result::Result<T, E>;

pub fn g_arithmetic(g: &mut GrammarDef<'_>) -> Result {
    // terminals.
    let lparen = g.terminal("LPAREN", None)?;
    let rparen = g.terminal("RPAREN", None)?;
    let plus = g.terminal("PLUS", None)?;
    let minus = g.terminal("MINUS", None)?;
    let star = g.terminal("STAR", None)?;
    let slash = g.terminal("SLASH", None)?;
    let num = g.terminal("NUM", None)?;

    // nonterminals.
    let expr = g.nonterminal("EXPR")?;
    let term = g.nonterminal("TERM")?;
    let factor = g.nonterminal("FACTOR")?;

    g.start_symbol(expr)?;

    // production rules.
    g.rule(expr, [expr, plus, term], "EXPR_ADD", None)?;
    g.rule(expr, [expr, minus, term], "EXPR_SUB", None)?;
    g.rule(expr, [term], "EXPR_TERM", None)?;

    g.rule(term, [term, star, factor], "TERM_MUL", None)?;
    g.rule(term, [term, slash, factor], "TERM_DIV", None)?;
    g.rule(term, [factor], "TERM_FACTOR", None)?;

    g.rule(factor, [num], "FACTOR_NUM", None)?;
    g.rule(factor, [lparen, expr, rparen], "FACTOR_PAREN", None)?;

    Ok(())
}

pub fn g_arithmetic_prec(g: &mut GrammarDef<'_>) -> Result {
    // precedences.
    let prec_add = Precedence::new(0, Assoc::Left);
    let prec_mul = Precedence::new(1, Assoc::Left);
    let prec_neg = Precedence::new(2, Assoc::Right);

    // terminal symbols.
    let lparen = g.terminal("LPAREN", None)?;
    let rparen = g.terminal("RPAREN", None)?;
    let plus = g.terminal("PLUS", Some(prec_add))?;
    let minus = g.terminal("MINUS", Some(prec_add))?;
    let star = g.terminal("STAR", Some(prec_mul))?;
    let slash = g.terminal("SLASH", Some(prec_mul))?;
    let num = g.terminal("NUM", None)?;

    // nonterminal symbols.
    let expr = g.nonterminal("EXPR")?;

    g.start_symbol(expr)?;

    // production rules.
    g.rule(expr, [expr, plus, expr], "EXPR_ADD", None)?;
    g.rule(expr, [expr, minus, expr], "EXPR_SUB", None)?;
    g.rule(expr, [expr, star, expr], "EXPR_MUL", None)?;
    g.rule(expr, [expr, slash, expr], "EXPR_DIV", None)?;
    g.rule(expr, [num], "EXPR_NUM", None)?;
    g.rule(expr, [lparen, expr, rparen], "EXPR_PAREN", None)?;
    g.rule(expr, [minus, expr], "EXPR_NEG", Some(prec_neg))?;

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

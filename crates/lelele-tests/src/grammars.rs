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
    let atom = g.nonterminal("ATOM")?;

    g.start_symbol(expr)?;

    // production rules.
    g.rule(expr, [expr, plus, term], None)?;
    g.rule(expr, [expr, minus, term], None)?;
    g.rule(expr, [term], None)?;

    g.rule(term, [term, star, factor], None)?;
    g.rule(term, [term, slash, factor], None)?;
    g.rule(term, [factor], None)?;

    g.rule(factor, [minus, factor], None)?;
    g.rule(factor, [atom], None)?;

    g.rule(atom, [num], None)?;
    g.rule(atom, [lparen, expr, rparen], None)?;

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
    g.rule(expr, [expr, plus, expr], None)?;
    g.rule(expr, [expr, minus, expr], None)?;
    g.rule(expr, [expr, star, expr], None)?;
    g.rule(expr, [expr, slash, expr], None)?;
    g.rule(expr, [num], None)?;
    g.rule(expr, [lparen, expr, rparen], None)?;
    g.rule(expr, [minus, expr], Some(prec_neg))?;

    Ok(())
}

pub fn g1(g: &mut GrammarDef<'_>) -> Result {
    let plus = g.terminal("PLUS", None)?;
    let star = g.terminal("STAR", None)?;
    let a = g.terminal("A", None)?;

    let e = g.nonterminal("E")?;
    let t = g.nonterminal("T")?;

    g.rule(e, [e, plus, t], None)?;
    g.rule(e, [t], None)?;

    g.rule(t, [t, star, a], None)?;
    g.rule(t, [a], None)?;

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

    g.rule(def, [param_spec, return_spec, comma], None)?;

    g.rule(param_spec, [type_], None)?;
    g.rule(param_spec, [name_list, colon, type_], None)?;

    g.rule(return_spec, [type_], None)?;
    g.rule(return_spec, [name, colon, type_], None)?;

    g.rule(type_, [ident], None)?;

    g.rule(name, [ident], None)?;

    g.rule(name_list, [name], None)?;
    g.rule(name_list, [name, comma, name_list], None)?;

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

    g.rule(e, [e, plus, t], None)?;
    g.rule(e, [t], None)?;

    g.rule(t, [lparen, e, rparen], None)?;
    g.rule(t, [num], None)?;

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

    g.rule(e, [e, plus, t], None)?;
    g.rule(e, [t], None)?;

    g.rule(t, [t, star, a], None)?;
    g.rule(t, [a], None)?;
    g.rule(t, [lparen, e, rparen], None)?;

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

    g.rule(e, [e, plus, t, n], None)?;
    g.rule(e, [t], None)?;

    g.rule(t, [a], None)?;
    g.rule(t, [lparen, e, n, rparen], None)?;
    g.rule(t, [n, a], None)?;

    g.rule(n, [], None)?;
    g.rule(n, [num], None)?;

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

    g.rule(s_s, [t_d, t_i, s_a], None)?;

    g.rule(s_a, [s_a, s_t], None)?;
    g.rule(s_a, [], None)?;

    g.rule(s_t, [s_m], None)?;
    g.rule(s_t, [s_y], None)?;
    g.rule(s_t, [s_p], None)?;
    g.rule(s_t, [s_b], None)?;

    g.rule(s_m, [t_r], None)?;
    g.rule(s_m, [t_c], None)?;

    g.rule(s_y, [t_x], None)?;
    g.rule(s_y, [t_f], None)?;

    g.rule(s_p, [t_n], None)?;
    g.rule(s_p, [t_o], None)?;

    g.rule(s_b, [t_a], None)?;
    g.rule(s_b, [t_e], None)?;

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

    g.rule(s_a, [s_a, s_b], None)?;
    g.rule(s_a, [s_b], None)?;

    g.rule(s_b, [s_c], None)?;

    g.rule(s_c, [s_d], None)?;
    g.rule(s_c, [t_x, t_y], None)?;

    g.rule(s_d, [t_s, s_e], None)?;

    g.rule(s_e, [s_e, t_plus, t_y], None)?;
    g.rule(s_e, [t_y], None)?;
    g.rule(s_e, [], None)?;

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

    g.rule(n_e, [n_e, t_plus, n_t], None)?;
    g.rule(n_e, [n_t], None)?;

    g.rule(n_t, [t_a], None)?;
    g.rule(n_t, [t_num], None)?;
    g.rule(n_t, [t_lparen, n_e, t_rparen], None)?;

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

    g.rule(n_e, [n_e, n_o, n_t], None)?;
    g.rule(n_e, [n_t], None)?;

    g.rule(n_t, [t_a], None)?;
    g.rule(n_t, [t_lparen, n_e, t_rparen], None)?;

    g.rule(n_o, [t_x], None)?;
    g.rule(n_o, [], None)?;

    Ok(())
}

// TODO: G11 ~ G17

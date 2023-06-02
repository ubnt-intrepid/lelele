//! Grammar definition for integration tests.

use lelele::grammar::{Choice, GrammarDef};

pub fn g_simple1(g: &mut GrammarDef<'_>) {
    let equal = g.token("EQUAL");
    let plus = g.token("PLUS");
    let ident = g.token("ID");
    let num = g.token("NUM");

    let a = g.symbol("A");
    let e = g.symbol("E");
    let t = g.symbol("T");

    g.start_symbol(a);

    g.rule(a, Choice(((e, equal, e), ident)));
    g.rule(e, Choice(((e, plus, t), t)));
    g.rule(t, Choice((num, ident)));
}

pub fn g_simple2(g: &mut GrammarDef<'_>) {
    // declare terminal symbols.
    let lparen = g.token("LPAREN");
    let rparen = g.token("RPAREN");
    let plus = g.token("PLUS");
    let minus = g.token("MINUS");
    let star = g.token("STAR");
    let slash = g.token("SLASH");
    let num = g.token("NUM");

    // declare nonterminal symbols.
    let expr = g.symbol("EXPR");
    let factor = g.symbol("FACTOR");
    let term = g.symbol("TERM");

    g.start_symbol(expr);

    // declare syntax rules.
    g.rule(
        expr,
        Choice((
            (expr, plus, factor),  // expr '+' factor
            (expr, minus, factor), // expr '-' factor
            factor,                // factor
        )),
    );
    g.rule(
        factor,
        Choice((
            (factor, star, term),  // factor '*' term
            (factor, slash, term), // factor '/' term
            term,                  // term
        )),
    );
    g.rule(
        term,
        Choice((
            num,                    // num
            (lparen, expr, rparen), // '(' expr ')'
        )),
    );
}

pub fn g1(g: &mut GrammarDef<'_>) {
    let plus = g.token("PLUS");
    let star = g.token("STAR");
    let a = g.token("A");

    let e = g.symbol("E");
    let t = g.symbol("T");

    g.start_symbol(e);

    g.rule(e, Choice(((e, plus, t), t)));
    g.rule(t, Choice(((t, star, a), a)));
}

pub fn g2(g: &mut GrammarDef<'_>) {
    let comma = g.token("COMMA");
    let colon = g.token("COLON");
    let ident = g.token("ID");

    let def = g.symbol("DEF");
    let param_spec = g.symbol("PARAM_SPEC");
    let return_spec = g.symbol("RETURN_SPEC");
    let type_ = g.symbol("TYPE");
    let name = g.symbol("NAME");
    let name_list = g.symbol("NAME_LIST");

    g.rule(def, (param_spec, return_spec, comma));
    g.rule(param_spec, Choice((type_, (name_list, colon, type_))));
    g.rule(return_spec, Choice((type_, (name, colon, type_))));
    g.rule(type_, ident);
    g.rule(name, ident);
    g.rule(name_list, Choice((name, (name, comma, name_list))));
}

// g3 is same as g2

pub fn g4(g: &mut GrammarDef<'_>) {
    let plus = g.token("PLUS");
    let lparen = g.token("LPAREN");
    let rparen = g.token("RPAREN");
    let num = g.token("NUM");

    let e = g.symbol("E");
    let t = g.symbol("T");
    // E → E + T | T
    // T → ( E ) | n

    g.rule(e, Choice(((e, plus, t), t)));
    g.rule(t, Choice(((lparen, e, rparen), num)));
}

// TODO: G5 ~ G17

pub fn min_caml(g: &mut GrammarDef<'_>) {
    // WIP

    let l_paren = g.token("LPAREN");
    let r_paren = g.token("RPAREN");
    let t_true = g.token("TRUE");
    let t_false = g.token("FALSE");
    let integer = g.token("INTEGER");
    let float = g.token("FLOAT");
    let ident = g.token("IDENT");
    let t_not = g.token("NOT");
    let plus = g.token("PLUS");
    let plus_dot = g.token("PLUS_DOT");
    let minus = g.token("MINUS");
    let minus_dot = g.token("MINUS_DOT");
    let star_dot = g.token("STAR_DOT");
    let slash_dot = g.token("SLASH_DOT");
    let equal = g.token("EQUAL");
    let less_greater = g.token("LESS_GREATER");
    let less = g.token("LESS");
    let greater = g.token("GREATER");
    let less_equal = g.token("LESS_EQUAL");
    let greater_equal = g.token("GREATER_EQUAL");
    let less_minus = g.token("LESS_MINUS");
    let comma = g.token("COMMA");
    let semicolon = g.token("SEMICOLON");
    let t_if = g.token("IF");
    let t_then = g.token("THEN");
    let t_else = g.token("ELSE");
    let t_let = g.token("LET");
    let t_rec = g.token("REC");
    let t_in = g.token("IN");
    let array_make = g.token("ARRAY_MAKE");
    let dot = g.token("DOT");

    let simple_exp = g.symbol("SIMPLE_EXP");
    let app_exp = g.symbol("APP_EXP");
    let neg_exp = g.symbol("NEG_EXP");
    let mult_exp = g.symbol("MULT_EXP");
    let add_exp = g.symbol("ADD_EXP");
    let rel_exp = g.symbol("REL_EXP");
    let tuple_exp = g.symbol("TUPLE_EXP");
    let put_exp = g.symbol("PUT_EXP");
    let if_exp = g.symbol("IF_EXP");
    let let_expr = g.symbol("LET_EXP");
    let exp = g.symbol("EXPR");
    let formal_args = g.symbol("FORMAL_ARGS");
    let actual_args = g.symbol("ACTUAL_ARGS");
    let tuple_exp_rest = g.symbol("TUPLE_EXP_REST");
    let fundef = g.symbol("FUNDEF");
    let pat = g.symbol("PAT");

    g.rule(
        simple_exp,
        Choice((
            (l_paren, exp, r_paren),
            (l_paren, r_paren),
            t_true,
            t_false,
            integer,
            float,
            ident,
            (simple_exp, dot, l_paren, exp, r_paren),
        )),
    );

    g.rule(
        app_exp,
        Choice((
            simple_exp, //
            (simple_exp, actual_args),
            (array_make, simple_exp, simple_exp),
            (t_not, app_exp),
        )),
    );

    g.rule(
        neg_exp,
        Choice((
            app_exp, //
            (minus, neg_exp),
            (minus_dot, neg_exp),
        )),
    );

    g.rule(
        mult_exp,
        Choice((
            neg_exp, //
            (mult_exp, star_dot, neg_exp),
            (mult_exp, slash_dot, neg_exp),
        )),
    );

    g.rule(
        add_exp,
        Choice((
            mult_exp, //
            (add_exp, plus, mult_exp),
            (add_exp, minus, mult_exp),
            (add_exp, plus_dot, mult_exp),
            (add_exp, minus_dot, mult_exp),
        )),
    );

    g.rule(
        rel_exp,
        Choice((
            add_exp, //
            (rel_exp, equal, add_exp),
            (rel_exp, less_greater, add_exp),
            (rel_exp, less, add_exp),
            (rel_exp, greater, add_exp),
            (rel_exp, less_equal, add_exp),
            (rel_exp, greater_equal, add_exp),
        )),
    );

    g.rule(
        tuple_exp,
        Choice((
            rel_exp, //
            (rel_exp, comma, tuple_exp_rest),
        )),
    );

    g.rule(
        tuple_exp_rest,
        Choice((
            rel_exp, //
            (rel_exp, comma, tuple_exp_rest),
        )),
    );

    g.rule(
        put_exp,
        Choice((
            tuple_exp,
            (simple_exp, dot, l_paren, exp, r_paren, less_minus, exp),
        )),
    );

    g.rule(
        if_exp,
        Choice((
            put_exp, //
            (t_if, exp, t_then, exp, t_else, exp),
        )),
    );

    g.rule(
        let_expr,
        Choice((
            (t_let, ident, equal, exp, t_in, exp), //
            (t_let, t_rec, fundef, t_in, exp),
            (t_let, l_paren, pat, r_paren, equal, exp, t_in, exp),
        )),
    );

    g.rule(fundef, (ident, formal_args, equal, exp));

    g.rule(
        formal_args,
        Choice((
            (ident, formal_args), //
            ident,
        )),
    );

    g.rule(
        actual_args,
        Choice((
            (actual_args, simple_exp), //
            simple_exp,
        )),
    );

    g.rule(
        pat,
        Choice((
            (pat, comma, ident), //
            (ident, comma, ident),
        )),
    );

    g.rule(
        exp,
        Choice((
            if_exp, //
            (if_exp, semicolon, exp),
            let_expr,
        )),
    );

    g.start_symbol(exp);
}

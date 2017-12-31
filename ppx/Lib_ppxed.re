[@ocaml.ppx.context {cookies: []}];

let fail = (loc, txt) => raise(Location.Error(Location.error(~loc, txt)));

let rec process_bindings = (bindings) =>
  Parsetree.(
    switch bindings {
    | [] => assert false
    | [binding] => (binding.pvb_pat, binding.pvb_expr)
    | [binding, ...rest] =>
      let (pattern, expr) = process_bindings(rest);
      (
        Ast_helper.Pat.tuple([binding.pvb_pat, pattern]),
        {
          Parsetree.pexp_desc:
            [@implicit_arity]
            Parsetree.Pexp_apply(
              {
                Parsetree.pexp_desc:
                  Parsetree.Pexp_ident({
                    Asttypes.txt:
                      [@implicit_arity] Longident.Ldot(Longident.Lident("Let_syntax"), "join2"),
                    Asttypes.loc: Pervasives.(^)(Ast_helper.default_loc)
                  }),
                Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
                Parsetree.pexp_attributes: []
              },
              [("", binding.pvb_expr), ("", expr)]
            ),
          Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
          Parsetree.pexp_attributes: []
        }
      )
    }
  );

let process_let = (contents, loc) => {
  open Parsetree;
  let bindings =
    switch contents {
    | PStr([{pstr_desc: Pstr_value(Nonrecursive, bindings), pstr_loc}]) => bindings
    | _ => fail(loc, "extension must contain a nonrecursive let binding")
    };
  process_bindings(bindings)
};

let getExpr = (contents, loc) =>
  Parsetree.(
    switch contents {
    | PStr([{pstr_desc: Pstr_eval(expr, _)}]) => expr
    | _ => fail(loc, "@else must contain an expression")
    }
  );

let mapper = (_argv) =>
  Parsetree.{
    ...Ast_mapper.default_mapper,
    expr: (mapper, expr) =>
      switch expr.pexp_desc {
      | Pexp_sequence(
          {pexp_desc: Pexp_extension(({txt: "map" | "awaitWrap"}, contents)), pexp_loc},
          next
        )
      | Pexp_sequence(
          {
            pexp_desc: Pexp_extension(({txt: "bind" | "await"}, contents)),
            pexp_loc,
            pexp_attributes: [({txt: "wrap"}, _), ..._]
          },
          next
        ) =>
        let (pat, expr) = process_let(contents, pexp_loc);
        {
          Parsetree.pexp_desc:
            [@implicit_arity]
            Parsetree.Pexp_apply(
              {
                Parsetree.pexp_desc:
                  Parsetree.Pexp_ident({
                    Asttypes.txt:
                      [@implicit_arity] Longident.Ldot(Longident.Lident("Let_syntax"), "map"),
                    Asttypes.loc: Pervasives.(^)(Ast_helper.default_loc)
                  }),
                Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
                Parsetree.pexp_attributes: []
              },
              [
                ("", expr),
                (
                  "f",
                  {
                    Parsetree.pexp_desc:
                      [@implicit_arity]
                      Parsetree.Pexp_fun("", None, pat, mapper.expr(mapper, next)),
                    Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
                    Parsetree.pexp_attributes: []
                  }
                )
              ]
            ),
          Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
          Parsetree.pexp_attributes: []
        }
      | Pexp_extension(({txt: "map" | "awaitWrap"}, contents)) =>
        let (pat, expr) = process_let(contents, expr.pexp_loc);
        {
          Parsetree.pexp_desc:
            [@implicit_arity]
            Parsetree.Pexp_apply(
              {
                Parsetree.pexp_desc:
                  Parsetree.Pexp_ident({
                    Asttypes.txt:
                      [@implicit_arity] Longident.Ldot(Longident.Lident("Let_syntax"), "map"),
                    Asttypes.loc: Pervasives.(^)(Ast_helper.default_loc)
                  }),
                Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
                Parsetree.pexp_attributes: []
              },
              [
                ("", expr),
                (
                  "f",
                  {
                    Parsetree.pexp_desc:
                      [@implicit_arity]
                      Parsetree.Pexp_fun(
                        "",
                        None,
                        pat,
                        {
                          Parsetree.pexp_desc:
                            [@implicit_arity]
                            Parsetree.Pexp_construct(
                              {
                                Asttypes.txt: Longident.Lident("()"),
                                Asttypes.loc: Pervasives.(^)(Ast_helper.default_loc)
                              },
                              None
                            ),
                          Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
                          Parsetree.pexp_attributes: []
                        }
                      ),
                    Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
                    Parsetree.pexp_attributes: []
                  }
                )
              ]
            ),
          Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
          Parsetree.pexp_attributes: []
        }
      | Pexp_sequence(
          {pexp_desc: Pexp_extension(({txt: "bind" | "await"}, contents)), pexp_loc},
          next
        ) =>
        let (pat, expr) = process_let(contents, pexp_loc);
        {
          Parsetree.pexp_desc:
            [@implicit_arity]
            Parsetree.Pexp_apply(
              {
                Parsetree.pexp_desc:
                  Parsetree.Pexp_ident({
                    Asttypes.txt:
                      [@implicit_arity] Longident.Ldot(Longident.Lident("Let_syntax"), "bind"),
                    Asttypes.loc: Pervasives.(^)(Ast_helper.default_loc)
                  }),
                Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
                Parsetree.pexp_attributes: []
              },
              [
                ("", expr),
                (
                  "f",
                  {
                    Parsetree.pexp_desc:
                      [@implicit_arity]
                      Parsetree.Pexp_fun("", None, pat, mapper.expr(mapper, next)),
                    Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
                    Parsetree.pexp_attributes: []
                  }
                )
              ]
            ),
          Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
          Parsetree.pexp_attributes: []
        }
      | Pexp_extension(({txt: "bind" | "await"}, contents)) =>
        let (pat, expr) = process_let(contents, expr.pexp_loc);
        {
          Parsetree.pexp_desc:
            [@implicit_arity]
            Parsetree.Pexp_apply(
              {
                Parsetree.pexp_desc:
                  Parsetree.Pexp_ident({
                    Asttypes.txt:
                      [@implicit_arity] Longident.Ldot(Longident.Lident("Let_syntax"), "bind"),
                    Asttypes.loc: Pervasives.(^)(Ast_helper.default_loc)
                  }),
                Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
                Parsetree.pexp_attributes: []
              },
              [
                ("", expr),
                (
                  "f",
                  {
                    Parsetree.pexp_desc:
                      [@implicit_arity]
                      Parsetree.Pexp_fun(
                        "",
                        None,
                        pat,
                        {
                          Parsetree.pexp_desc:
                            [@implicit_arity]
                            Parsetree.Pexp_construct(
                              {
                                Asttypes.txt: Longident.Lident("()"),
                                Asttypes.loc: Pervasives.(^)(Ast_helper.default_loc)
                              },
                              None
                            ),
                          Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
                          Parsetree.pexp_attributes: []
                        }
                      ),
                    Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
                    Parsetree.pexp_attributes: []
                  }
                )
              ]
            ),
          Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
          Parsetree.pexp_attributes: []
        }
      | Pexp_sequence(
          {pexp_desc: Pexp_extension(({txt: "consume"}, contents)), pexp_loc, pexp_attributes},
          next
        ) =>
        let (pat, expr) = process_let(contents, pexp_loc);
        let thenn =
          switch pexp_attributes {
          | [({txt: "then"}, contents)] => getExpr(contents, pexp_loc)
          | _ => {
              Parsetree.pexp_desc:
                [@implicit_arity]
                Parsetree.Pexp_construct(
                  {
                    Asttypes.txt: Longident.Lident("()"),
                    Asttypes.loc: Pervasives.(^)(Ast_helper.default_loc)
                  },
                  None
                ),
              Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
              Parsetree.pexp_attributes: []
            }
          };
        {
          Parsetree.pexp_desc:
            [@implicit_arity]
            Parsetree.Pexp_sequence(
              {
                Parsetree.pexp_desc:
                  [@implicit_arity]
                  Parsetree.Pexp_apply(
                    {
                      Parsetree.pexp_desc:
                        Parsetree.Pexp_ident({
                          Asttypes.txt:
                            [@implicit_arity]
                            Longident.Ldot(Longident.Lident("Let_syntax"), "consume"),
                          Asttypes.loc: Pervasives.(^)(Ast_helper.default_loc)
                        }),
                      Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
                      Parsetree.pexp_attributes: []
                    },
                    [
                      ("", expr),
                      (
                        "f",
                        {
                          Parsetree.pexp_desc:
                            [@implicit_arity]
                            Parsetree.Pexp_fun("", None, pat, mapper.expr(mapper, next)),
                          Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
                          Parsetree.pexp_attributes: []
                        }
                      )
                    ]
                  ),
                Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
                Parsetree.pexp_attributes: []
              },
              thenn
            ),
          Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
          Parsetree.pexp_attributes: []
        }
      | Pexp_extension(({txt: "consume"}, contents)) =>
        let (pat, expr) = process_let(contents, expr.pexp_loc);
        let thenn =
          switch expr.pexp_attributes {
          | [({txt: "then"}, contents)] => getExpr(contents, expr.pexp_loc)
          | _ => {
              Parsetree.pexp_desc:
                [@implicit_arity]
                Parsetree.Pexp_construct(
                  {
                    Asttypes.txt: Longident.Lident("()"),
                    Asttypes.loc: Pervasives.(^)(Ast_helper.default_loc)
                  },
                  None
                ),
              Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
              Parsetree.pexp_attributes: []
            }
          };
        {
          Parsetree.pexp_desc:
            [@implicit_arity]
            Parsetree.Pexp_sequence(
              {
                Parsetree.pexp_desc:
                  [@implicit_arity]
                  Parsetree.Pexp_apply(
                    {
                      Parsetree.pexp_desc:
                        Parsetree.Pexp_ident({
                          Asttypes.txt:
                            [@implicit_arity]
                            Longident.Ldot(Longident.Lident("Let_syntax"), "consume"),
                          Asttypes.loc: Pervasives.(^)(Ast_helper.default_loc)
                        }),
                      Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
                      Parsetree.pexp_attributes: []
                    },
                    [
                      ("", expr),
                      (
                        "f",
                        {
                          Parsetree.pexp_desc:
                            [@implicit_arity]
                            Parsetree.Pexp_fun(
                              "",
                              None,
                              pat,
                              {
                                Parsetree.pexp_desc:
                                  [@implicit_arity]
                                  Parsetree.Pexp_construct(
                                    {
                                      Asttypes.txt: Longident.Lident("()"),
                                      Asttypes.loc: Pervasives.(^)(Ast_helper.default_loc)
                                    },
                                    None
                                  ),
                                Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
                                Parsetree.pexp_attributes: []
                              }
                            ),
                          Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
                          Parsetree.pexp_attributes: []
                        }
                      )
                    ]
                  ),
                Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
                Parsetree.pexp_attributes: []
              },
              thenn
            ),
          Parsetree.pexp_loc: Pervasives.(^)(Ast_helper.default_loc),
          Parsetree.pexp_attributes: []
        }
      | _ => Ast_mapper.default_mapper.expr(mapper, expr)
      }
  };

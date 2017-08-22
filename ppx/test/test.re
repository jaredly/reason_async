open Migrate_parsetree.Ast_403;

let module Convert = Migrate_parsetree.Convert(Migrate_parsetree.OCaml_403)(Migrate_parsetree.OCaml_current);

let show_structure structure => {
  Pprintast.structure Format.str_formatter (Convert.copy_structure structure);
  Format.flush_str_formatter();
};

let debug_structure structure => {
  Printast.structure 0 Format.str_formatter(Convert.copy_structure structure);
  Format.flush_str_formatter()
};

let show_error input result expected => {
  print_endline ">> Input:";
  print_endline (show_structure input);
  print_endline (debug_structure input);
  print_endline ">> Output:";
  print_endline (show_structure result);
  print_endline ">> Expected:";
  print_endline (show_structure expected);
};

let fixtures = [(
  [%str
    let getIntParam req param => {
      let module Let_syntax = Monads.Option;
      [%bind let value = Js.Dict.get (Request.query req) param];
      [%map let float = Js.Json.decodeNumber value];
      int_of_float float
    };
  ],
  [%str
    let getIntParam req param => {
      let module Let_syntax = Monads.Option;
      Let_syntax.bind (Js.Dict.get (Request.query req) param) f::(fun value => {
        Let_syntax.map (Js.Json.decodeNumber value) f::(fun float => {
          int_of_float float
        })
      })
    };
  ]
), (
  [%str
    let full2 db uid => {
      let module Let_syntax = Db.Async;
      [%consume let res = dump db uid];
      switch res {
        | Ok data => Js.log data
        | Error err => Js.log ("Failed" ^ err)
      }
    };
  ], [%str
    let full2 db uid => {
      let module Let_syntax = Db.Async;
      Let_syntax.consume (dump db uid) f::(fun res => {
      switch res {
        | Ok data => Js.log data
        | Error err => Js.log ("Failed" ^ err)
      }
      });
      ()
    };
  ]
), (
  [%str
    let example () => {
      [%await let x = Db.get "one"
      and y = Db.get "two"
      and z = Db.get "three"][@wrap];
      /* now x, y, and z are all available */
      (x, y, z)
    }
  ], [%str
    let example () => {
      Let_syntax.map (Let_syntax.join2 (Db.get "one") (Let_syntax.join2 (Db.get "two") (Db.get "three"))) f::(fun (x, (y, z)) => {
        /* now x, y, and z are all available */
        (x, y, z)
      })
    }
  ]
)];

let run () => {
  let (total, failures) =
  List.fold_left (fun (total, failures) (input, expected) => {
    try {
    let result = Lib.mapper.structure Lib.mapper input;
    if (result != expected) {
      show_error input result expected;
      (total + 1, failures + 1)
    } else {
      (total + 1, failures)
    }
    } {
      | Location.Error error => {
        print_endline ">> Input:";
        print_endline (show_structure input);
        print_endline ">> Error:";
        print_endline error.Location.msg;
        (total + 1, failures + 1)
      }
    }
  }) (0, 0) fixtures;

  if (failures !== 0) {
      Printf.printf "Total: %d, Failures: %d" total failures;
    exit 1;
  } else {
    Printf.printf "All %d succeeded!" total;
    exit 0;
  }
};

run ();

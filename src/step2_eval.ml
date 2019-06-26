module E = Env
module T = Types
module TT = Types.Types


exception EvalErr of string


let read str =
  Reader.read_str str


let rec eval env ast =
  let eval_ast env ast =
    match ast with
    | TT.Symbol(x, _) ->
      begin match E.get x env with
        | Some(v) ->
          v

        | None ->
          raise (EvalErr ("can't find symbol '" ^ x ^ "'."))
      end

    | TT.List(xs, _) ->
      T.list(List.map (eval env) xs)

    | TT.Vector(xs, _) ->
      T.vector(List.map (eval env) xs)

    | TT.Map(xs, _) ->
      T.map(
        T.MalMap.fold
          (fun k v m -> T.MalMap.add (eval env k) (eval env v) m)
          xs
          T.MalMap.empty
      )

    | _ ->
      ast
  in

  match eval_ast env ast with
  | TT.List(TT.Fn(f, _) :: args, _) ->
    f args

  | _ as result ->
    result


let print exp =
  Printer.print_str exp


let rep str =
  let arith_fn f =
    T.fn(
      function
      | [TT.Int(a); TT.Int(b)] ->
        TT.Int(f a b)

      | _ ->
        raise (EvalErr "Arithmetic functions require two int arguments.")
    ) in

  let repl_env =
    let env = E.root () in
    E.set "+" (arith_fn ( + )) env ;
    E.set "-" (arith_fn ( - )) env ;
    E.set "*" (arith_fn ( * )) env ;
    E.set "/" (arith_fn ( / )) env ;
    env in

  str |> read |> eval repl_env |> print


let main =
  let print_err s =
    print_endline ("Error: " ^ s)
  in
  try
    while true do
      print_string "user> " ;
      try
        rep (read_line ()) ;
      with 
      | Reader.Nothing ->
        ()

      | Reader.ReaderErr err ->
        print_err err

      | EvalErr s ->
        print_err s
    done
  with End_of_file ->
    ()

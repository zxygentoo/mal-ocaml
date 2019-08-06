module E = Env
module T = Types


exception Err of string


let read str =
  Reader.read_str str


let rec eval env ast =
  match eval_ast env ast with
  | T.List(T.Fn(f, _) :: args, _) ->
    f args

  | _ as result ->
    result

and eval_ast env ast =
  match ast with
  | T.Symbol(x, _) ->
    begin match E.get x env with
      | Some(v) ->
        v

      | None ->
        raise (Err ("can't find symbol '" ^ x ^ "'."))
    end

  | T.List(xs, _) ->
    T.list(List.map (eval env) xs)

  | T.Vector(xs, _) ->
    T.vector(List.map (eval env) xs)

  | T.Map(xs, _) ->
    T.map(
      T.MalMap.fold
        (fun k v m -> T.MalMap.add (eval env k) (eval env v) m)
        xs
        T.MalMap.empty
    )

  | _ ->
    ast


let print exp =
  Printer.print_str exp


let rep str =
  let arith_fn f =
    T.fn(
      function
      | [T.Int(a) ; T.Int(b)] ->
        T.Int(f a b)

      | _ ->
        raise (Err "Arithmetic functions require two int arguments.")
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

      | Reader.Err err ->
        print_err err

      | Err msg ->
        print_err msg
    done
  with
  | End_of_file ->
    ()

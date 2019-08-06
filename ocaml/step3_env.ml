module E = Env
module T = Types


exception Err of string


let read str =
  Reader.read_str str


let rec eval env ast =
  match ast with
  | T.List([], _) ->
    ast

  | T.List(T.Symbol(sym, _) :: expr, _) ->
    begin match sym with
      | "def!" ->
        eval_def env expr

      | "let*" ->
        eval_let env expr

      | _ ->
        apply_fn env ast
    end

  | _  ->
    eval_ast env ast

and eval_def env =
  function
  | [T.Symbol(sym, _) ; expr] ->
    let value = eval env expr in
    E.set sym value env ;
    value

  | _ ->
    raise (Err "Invalid 'def!' form.")

and eval_let env =
  function
  | [T.List(bindings, _) ; body]

  | [T.Vector(bindings, _) ; body] ->
    let let_env = E.make (Some env) in
    let rec eval_let_bindings =
      function
      | T.Symbol(k, _) :: expr :: rest ->
        E.set k (eval let_env expr) let_env ;
        eval_let_bindings rest

      | _ :: _ :: _ ->
        raise (Err "'let*' binding first element should be a symbol.")

      | _ :: [] ->
        raise (Err "'let*' bindings must have even number of elements.")

      | [] ->
        ()
    in
    eval_let_bindings bindings ;
    eval let_env body

  | _ ->
    raise (Err "Invalid 'let*' form.")

and apply_fn env ast =
  match eval_ast env ast with
  | T.List(T.Fn(f, _) :: args, _) ->
    f args

  | _ ->
    raise (Err "Can't invoke non-function.")

and eval_ast env =
  function
  | T.Symbol(x, _) ->
    begin match E.get x env with
      | Some(v) ->
        v

      | None ->
        raise (Err ("Symbol '" ^ x ^ "' not found."))
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

  | _ as ast ->
    ast


let print exp =
  Printer.print_str exp


let repl_env =
  let arith_fn f =
    T.fn(
      function
      | [T.Int(a) ; T.Int(b)] ->
        T.Int(f a b)

      | _ ->
        raise (Err "Arithmetic functions require two int arguments.")
    ) in

  let env = E.root () in
  E.set "+" (arith_fn ( + )) env ;
  E.set "-" (arith_fn ( - )) env ;
  E.set "*" (arith_fn ( * )) env ;
  E.set "/" (arith_fn ( / )) env ;
  env


let rep str =
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

      | Reader.Err msg ->
        print_err msg

      | Err msg ->
        print_err msg
    done
  with
  | End_of_file ->
    ()

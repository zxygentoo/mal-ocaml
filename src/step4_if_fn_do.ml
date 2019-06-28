module E = Env
module T = Types
module TT = Types.Types


exception Err of string


(* helpers *)

let red s =
  "\027[31m" ^ s ^ "\027[0m"

let print_err msg =
  print_endline (red ("Error: " ^ msg))


(* REP functions *)

let read str =
  Reader.read_str str


let rec eval env ast =
  match ast with
  | TT.List([], _) ->
    ast

  | TT.List(TT.Symbol("def!", _) :: expr, _) ->
    eval_def env expr

  | TT.List(TT.Symbol("let*", _) :: expr, _) ->
    eval_let env expr

  | TT.List(TT.Symbol("do", _) :: expr, _) ->
    eval_do env expr

  | TT.List(TT.Symbol("if", _) :: expr, _) ->
    eval_if env expr

  | TT.List(TT.Symbol("fn*", _) :: expr, _) ->
    eval_fn env expr

  | TT.List(_, _) ->
    apply_function env ast

  | _  ->
    eval_ast env ast

and eval_def env =
  function
  | [TT.Symbol(sym, _); expr] ->
    let value = eval env expr in
    E.set sym value env ;
    value

  | _ ->
    raise (Err "Invalid 'def!' form.")

and eval_let env =
  function
  | [TT.List(bindings, _); body]

  | [TT.Vector(bindings, _); body] ->
    let let_env = E.make (Some env) in
    let rec eval_let_bindings =
      function
      | TT.Symbol(k, _) :: expr :: rest ->
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

and eval_do env =
  function
  | [] ->
    T.nil

  | expr :: [] ->
    eval env expr

  | expr :: rest ->
    eval env expr |> ignore ;
    eval_do env rest

and eval_if env =
  function
  | [pred ; then_expr] ->
    if T.to_bool (eval env pred)
    then (eval env then_expr)
    else T.nil

  | [pred; then_expr; else_expr] ->
    if T.to_bool (eval env pred)
    then (eval env then_expr)
    else (eval env else_expr)

  | _ ->
    raise (Err "Invalid 'if' form.")

and eval_fn env =
  function
  | [TT.List(arg_names, _); body]

  | [TT.Vector(arg_names, _); body] ->
    T.fn(
      fun args ->
        let fn_env = E.make (Some env) in
        let rec bind_args ks vs =
          match (ks, vs) with
          | [TT.Symbol("&", _); TT.Symbol(name, _)], vs ->
            E.set name (T.list vs) fn_env

          | TT.Symbol(k, _) :: ks, v :: vs ->
            E.set k v fn_env ;
            bind_args ks vs

          | [], [] ->
            ()

          | _ ->
            raise (Err "Bad parameters count in 'fn*'.")
        in
        bind_args arg_names args ;
        eval fn_env body)

  | _ ->
    raise (Err "Invalid 'fn*' from.")

and apply_function env ast =
  match eval_ast env ast with
  | TT.List(TT.Fn(f, _) :: args, _) ->
    f args

  | _ ->
    raise (Err "Can't invoke non-function.")

and eval_ast env =
  function
  | TT.Symbol(x, _) ->
    begin match E.get x env with
      | Some(v) ->
        v

      | None ->
        raise (Err ("can't find symbol '" ^ x ^ "'."))
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
        T.MalMap.empty)

  | _ as scalar ->
    scalar


let print exp =
  Printer.print_str exp


let rep env str =
  str |> read |> eval env |> print


(* REPL entry *)

let main =
  let repl_env = Core.init (E.root ()) in
  rep repl_env "(def! not (fn* (a) (if a false true)))" |> ignore ;
  try
    while true do
      print_string "user> " ;
      try
        rep repl_env (read_line ()) ;
      with 
      | Reader.Nothing ->
        ()

      | Reader.Err msg ->
        print_err msg

      | Err msg ->
        print_err msg

      | Core.Err msg ->
        print_err msg
    done
  with
  | End_of_file ->
    ()

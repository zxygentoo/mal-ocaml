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
    apply_fn env ast

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
    eval
      (make_let_env (E.make (Some env)) bindings)
      body

  | _ ->
    raise (Err "Invalid 'let*' form.")

and make_let_env let_env =
  function
  | TT.Symbol(k, _) :: v :: bindings_left ->
    E.set k (eval let_env v) let_env ;
    make_let_env let_env bindings_left

  | _ :: _ :: _ ->
    raise (Err "'let*' binding first element should be a symbol.")

  | _ :: [] ->
    raise (Err "'let*' bindings must have even number of elements.")

  | [] ->
    let_env

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
  | [TT.List(arg_syms, _); body]

  | [TT.Vector(arg_syms, _); body] ->
    T.fn(
      fun args ->
        eval
          (make_fn_env (E.make (Some env)) arg_syms args)
          body)

  | _ ->
    raise (Err "Invalid 'fn*' from.")

and make_fn_env fn_env arg_syms args =
  match (arg_syms, args) with
  | [TT.Symbol("&", _); TT.Symbol(k, _)], vs ->
    E.set k (T.list vs) fn_env ;
    fn_env

  | TT.Symbol(k, _) :: syms, v :: vs ->
    E.set k v fn_env ;
    make_fn_env fn_env syms vs

  | [], [] ->
    fn_env

  | _ ->
    raise (Err "Invalid number of parameters in 'fn*' form.")

and apply_fn env ast =
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
        raise (Err ("Symbol '" ^ x ^ "' not found."))
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


let re env s =
  s |> read |> eval env


let rep env s =
  s |> read |> eval env |> print


(* mal definitions *)

let not_def = "(def! not (fn* (a) (if a false true)))"
let load_file_def = "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))"


(* REPL entry *)

let main =
  let repl_env = Core.init (E.root ()) in

  E.set "*ARGV*"
    (Types.list
       (if Array.length Sys.argv > 1
        then (List.map
                (fun x -> TT.String x)
                (List.tl (List.tl (Array.to_list Sys.argv))))
        else []))
    repl_env ;

  E.set "eval"
    (T.fn(
        function
        | [ast] -> eval repl_env ast
        | _ -> raise (Core.Err "Invalid arguments for 'eval'.")))
    repl_env ;

  re repl_env not_def |> ignore ;
  re repl_env load_file_def |> ignore ;

  if Array.length Sys.argv > 1 then
    re repl_env ("(load-file \"" ^ Sys.argv.(1) ^ "\")") |> ignore
  else
    try
      while true do
        print_string "user> " ;
        try
          rep repl_env (read_line ()) ;
        with 
        | Reader.Nothing ->
          ()

        | Reader.Err msg | Core.Err msg | Err msg ->
          print_err msg
      done
    with
    | End_of_file ->
      ()

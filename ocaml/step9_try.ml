module E = Env
module T = Types


exception Err of string


let read str =
  Reader.read_str str


let rec eval env ast' =
  match macroexpand env ast' with
  | T.List([], _) as ast ->
    ast

  | T.List(T.Symbol("def!", _) :: expr, _) ->
    eval_def env expr

  | T.List(T.Symbol("defmacro!", _) :: expr, _) ->
    eval_defmacro env expr

  | T.List(T.Symbol("let*", _) :: expr, _) ->
    eval_let env expr

  | T.List(T.Symbol("do", _) :: expr, _) ->
    eval_do env expr

  | T.List(T.Symbol("if", _) :: expr, _) ->
    eval_if env expr

  | T.List(T.Symbol("fn*", _) :: expr, _) ->
    eval_fn env expr

  | T.List(T.Symbol("quote", _) :: ast, _) ->
    eval_quote ast

  | T.List(T.Symbol("quasiquote", _) :: ast, _) ->
    eval_quasiquote env ast

  | T.List(T.Symbol("macroexpand", _) :: ast, _) ->
    eval_macroexpand env ast

  | T.List(T.Symbol("try*", _) :: ast, _) ->
    eval_try env ast

  | T.List(_, _) as ast ->
    apply_fn env ast

  | _ as ast ->
    eval_ast env ast

and macroexpand env =
  function
  | T.List(T.Symbol(x, _) :: args, _) as ast ->
    begin match E.get x env with
      | Some(Fn(f, _) as fn) when T.is_macro fn ->
        macroexpand env (f args)

      | _ ->
        ast
    end

  | _ as ast ->
    ast

and eval_def env =
  function
  | [ T.Symbol(sym, _) ; expr ] ->
    let value = eval env expr in
    E.set sym value env ;
    value

  | _ ->
    raise (Err "Invalid 'def!' form.")

and eval_defmacro env =
  function
  | [ T.Symbol(sym, _) ; expr ] ->
    begin match eval env expr with
      | T.Fn _ as fn ->
        let macro = T.set_macro fn in
        E.set sym macro env ;
        macro

      | _ ->
        raise (Err "Invalid 'defmacro!' form: must be a function.")
    end

  | _ ->
    raise (Err "Invalid 'def!' form.")

and eval_let env =
  function
  | [ T.List(bindings, _) ; body ]

  | [ T.Vector(bindings, _) ; body ] ->
    eval
      (make_let_env (E.make (Some env)) bindings)
      body

  | _ ->
    raise (Err "Invalid 'let*' form.")

and make_let_env let_env =
  function
  | T.Symbol(k, _) :: v :: bindings_left ->
    E.set k (eval let_env v) let_env ;
    make_let_env let_env bindings_left

  | _ :: _ :: _ ->
    raise (Err "'let*' binding first element should be a symbol.")

  | _ :: [] ->
    raise (Err "'let*' bindings must have even number of elements.")

  | [] ->
    let_env

and eval_do env =
  List.fold_left (fun _ expr -> eval env expr) T.nil

and eval_if env =
  function
  | [pred ; then_] ->
    if T.to_bool (eval env pred) then
      eval env then_
    else
      T.nil

  | [pred ; then_ ; else_] ->
    if T.to_bool (eval env pred) then
      eval env then_
    else
      eval env else_

  | _ ->
    raise (Err "Invalid 'if' form.")

and eval_fn env =
  function
  | [T.List(arg_syms, _) ; body]

  | [T.Vector(arg_syms, _) ; body] ->
    T.fn(
      fun args ->
        eval
          (make_fn_env (E.make (Some env)) arg_syms args)
          body)

  | _ ->
    raise (Err "Invalid 'fn*' from.")

and make_fn_env fn_env arg_syms args =
  match (arg_syms, args) with
  | [ T.Symbol("&", _) ; T.Symbol(k, _) ], vs ->
    E.set k (T.list vs) fn_env ;
    fn_env

  | T.Symbol(k, _) :: syms, v :: vs ->
    E.set k v fn_env ;
    make_fn_env fn_env syms vs

  | [], [] ->
    fn_env

  | _ ->
    raise (Err "Invalid number of arguments for function.")

and eval_quote =
  function
  | [ast] ->
    ast

  | _ ->
    raise (Err "Invalid 'quote' form.")

and eval_quasiquote env =
  function
  | [x] ->
    eval env (quasiquote x)

  | _ ->
    raise (Err "Invalid 'quasiquote' form.")

and quasiquote =
  function
  | T.List([ T.Symbol("unquote", _) ; ast ], _)

  | T.Vector([ T.Symbol("unquote", _) ; ast ], _) ->
    ast

  | T.List(T.List([ T.Symbol("splice-unquote", _) ; x ], _) :: xs, _)

  | T.Vector(T.List([ T.Symbol("splice-unquote", _) ; x ], _) :: xs, _) ->
    T.list [T.symbol "concat" ; x ; quasiquote (T.list xs)]

  | T.List(x :: xs, _)

  | T.Vector(x :: xs, _) ->
    T.list [T.symbol "cons" ; quasiquote x ; quasiquote (T.list xs)]

  | _ as ast ->
    T.list [T.symbol "quote" ; ast]

and eval_macroexpand env =
  function
  | [ast] ->
    macroexpand env ast

  | _ ->
    raise (Err "Invalid 'macroexpand' form.")

and eval_try env =
  function
  | [exc_expr] ->
    eval env exc_expr

  | [ expr_may_exc
    ; T.List([ T.Symbol("catch*", _)
             ; T.Symbol(exc, _)
             ; handler], _)
    ] ->
    begin
      try (eval env expr_may_exc)
      with exn ->
        let value = match exn with
          | Types.MalExn v ->
            v

          | T.Err msg | Reader.Err msg | Core.Err msg | Err msg ->
            T.string msg

          | e ->
            T.String (Printexc.to_string e)
        in
        let sub_env = Env.make (Some env) in
        Env.set exc value sub_env ;
        eval sub_env handler
    end

  | _ ->
    raise (Err "Invalid 'try*' form.")

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
        (* raise (Err ("Symbol '" ^ x ^ "' not found.")) *)
        (* mal test will fail on above msg *)
        raise (Err ("'" ^ x ^ "' not found"))
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
        T.MalMap.empty)

  | _ as ast ->
    ast


let print exp =
  Printer.print_str exp


let re env s =
  s |> read |> eval env


let rep env s =
  s |> re env |> print


(* helpers *)

let red s =
  "\027[31m" ^ s ^ "\027[0m"


let print_err msg =
  print_endline (red ("Error: " ^ msg))


(* mal definitions *)

let not_def = "(def! not (fn* (a) (if a false true)))"
let load_file_def = "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \")\")))))"
let cond_def = "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))"
let or_def = "(defmacro! or (fn* (& xs) (if (empty? xs) nil (if (= 1 (count xs)) (first xs) `(let* (or_FIXME ~(first xs)) (if or_FIXME or_FIXME (or ~@(rest xs))))))))"

(* REPL entry *)

let main =
  let repl_env = Core.init (E.root ()) in

  E.set "*ARGV*"
    (Types.list
       (if Array.length Sys.argv > 1 then
          (List.map
             (fun x -> T.String x)
             (List.tl (List.tl (Array.to_list Sys.argv))))
        else
          []))
    repl_env ;

  E.set "eval"
    (T.fn(
        function
        | [ast] -> eval repl_env ast
        | _ -> raise (Core.Err "'eval' takes only one argument.")))
    repl_env ;

  re repl_env not_def |> ignore ;
  re repl_env load_file_def |> ignore ;
  re repl_env cond_def |> ignore ;
  re repl_env or_def |> ignore ;

  if Array.length Sys.argv > 1 then
    try
      re repl_env ("(load-file \"" ^ Sys.argv.(1) ^ "\")") |> ignore
    with
    | Types.MalExn exc ->
      print_endline
        ("Exception: " ^ (Printer.string_of_maltype true exc) ^ "\n")

    | End_of_file ->
      ()

  else begin
    try
      while true do
        print_string "user> " ;
        try
          rep repl_env (read_line ()) ;
        with 
        | Reader.Nothing ->
          ()

        | T.MalExn exc ->
          print_endline
            ("Exception: " ^ (Printer.string_of_maltype true exc))

        | T.Err msg | Reader.Err msg | Core.Err msg | Err msg ->
          print_err msg
      done
    with
    | End_of_file ->
      ()
  end

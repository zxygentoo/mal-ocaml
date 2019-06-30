module T = Types
module TT = Types.Types
module E = Env


exception Err of string


let slurp filename =
  let chan = open_in filename in
  let b = Buffer.create 80 in
  Buffer.add_channel b chan (in_channel_length chan) ;
  close_in chan ;
  Buffer.contents b


let arith_fn f =
  function
  | [TT.Int(a) ; TT.Int(b)] -> TT.Int(f a b)
  | _ -> raise (Err "Arithmetic functions require two int arguments.")


let comp_fn f =
  function
  | [TT.Int(a) ; TT.Int(b)] -> TT.Bool(f a b)
  | _ -> raise (Err "Arithmetic comparisons require two int arguments.")


let add_core_defs env =
  let pr_str = Printer.string_of_maltype in
  let set s f =
    E.set s (T.fn f) env in
  begin
    (* arithmetic operations *)

    set "+"
      (arith_fn ( + )) ;

    set "-"
      (arith_fn ( - )) ;

    set "*"
      (arith_fn ( * )) ;

    set "/"
      (arith_fn ( / )) ;

    (* arithmentic comparisions *)

    set "<"
      (comp_fn ( < )) ;

    set "<="
      (comp_fn ( <= )) ;

    set ">"
      (comp_fn ( > )) ;

    set ">="
      (comp_fn ( >= )) ;

    (* printing related *)

    set "pr-str"
      (fun v ->
         TT.String (String.concat " " (List.map (pr_str true) v))) ;

    set "str"
      (fun v ->
         TT.String (String.concat "" (List.map (pr_str false) v))) ;

    set "prn"
      (fun v ->
         print_endline (String.concat " " (List.map (pr_str true) v)) ;
         T.nil) ;

    set "println"
      (fun v ->
         print_endline (String.concat " " (List.map (pr_str false) v)) ;
         T.nil) ;

    (* reading related *)

    set "read-string"
      (function [TT.String x] -> Reader.read_str x | _ -> T.nil) ;

    set "slurp"
      (function
        | [TT.String(s)] -> T.string(slurp s)
        | _ -> raise (Err "input for 'slurp' must be a single string.")) ;

    (* type testing and value construction *)

    set "list"
      (fun v -> T.list v) ;

    set "list?"
      (function
        | [TT.List _] -> T.maltrue
        | _ -> T.malfalse) ;

    set "atom"
      (function
        | [v] -> T.atom v
        | _ -> raise (Err "Invalid arguments for 'atom'.")) ;

    set "atom?"
      (function
        | [TT.Atom _] -> T.maltrue
        | _ -> T.malfalse) ;

    (* atom related *)

    set "deref"
      (function
        | [TT.Atom(x)] -> !x
        | _ -> raise (Err "Invalid arguments for 'deref'.")) ;

    set "reset!"
      (function
        | [TT.Atom(x) ; v] -> x := v ; v
        | _ -> raise (Err "Invalid arguments for 'reset!'.")) ;

    set "swap!"
      (function
        | TT.Atom(x) :: TT.Fn(f, _) :: args ->
          let v = f (!x :: args) in x := v ; v
        | _ -> raise (Err "Invalid arguments for 'swap!'.")) ;

    (* equality *)

    set "="
      (function
        | [a ; b] -> TT.Bool (T.mal_equal a b)
        | _ -> T.malfalse) ;

    (* misc *)

    set "empty?"
      (function
        | [TT.List([], _)]
        | [TT.Vector([], _)] -> T.maltrue
        | _ -> T.malfalse) ;

    set "count"
      (function
        | [TT.List(v, _)]
        | [TT.Vector(v, _)] -> T.int (List.length v)
        | _ -> T.int 0) ;
  end


let init env =
  add_core_defs env ;
  env

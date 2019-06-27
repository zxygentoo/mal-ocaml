module T = Types
module TT = Types.Types
module E = Env


exception Err of string


let arith_fn f =
  function
  | [TT.Int(a); TT.Int(b)] -> TT.Int(f a b)
  | _ -> raise (Err "Arithmetic functions require two int arguments.")


let comp_fn f =
  function
  | [TT.Int(a); TT.Int(b)] -> TT.Bool(f a b)
  | _ -> raise (Err "Arithmetic compare require two int arguments.")  


let add_core_fns env =
  let pr_str = Printer.string_of_maltype in
  let set s f = E.set s (T.fn f) env in
  begin
    set "+"
      (arith_fn ( + )) ;

    set "-"
      (arith_fn ( - )) ;

    set "*"
      (arith_fn ( * )) ;

    set "/"
      (arith_fn ( / )) ;

    set "<"
      (comp_fn ( < )) ;

    set "<="
      (comp_fn ( <= )) ;

    set ">"
      (comp_fn ( > )) ;

    set ">="
      (comp_fn ( >= )) ;

    set "pr-str"
      (function v ->
         TT.String (String.concat " " (List.map (pr_str true) v))) ;

    set "str"
      (function v ->
         TT.String (String.concat " " (List.map (pr_str false) v))) ;

    set "prn"
      (function v ->
         print_endline (String.concat " " (List.map (pr_str true) v)) ;
         T.nil) ;

    set "println"
      (function v ->
         print_endline (String.concat " " (List.map (pr_str false) v)) ;
         T.nil) ;

    set "list"
      (fun v -> T.list v) ;

    set "list?"
      (function
        | [TT.List _] -> TT.Bool true
        | _ -> TT.Bool false) ;

    set "empty?"
      (function
        | [TT.List([], _)]
        | [TT.Vector([], _)] -> TT.Bool true
        | _ -> TT.Bool false) ;

    set "count"
      (function
        | [TT.List(v, _)]
        | [TT.Vector(v, _)] -> T.int (List.length v)
        | _ -> T.int 0) ;

    set "="
      (function
        | [a; b] -> TT.Bool (T.mal_equal a b)
        | _ -> T.malfalse) ;
  end


let core_ns =
  let env = E.root () in
  add_core_fns env ;
  env

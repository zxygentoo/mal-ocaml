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
      (function xs ->
         TT.String
           (String.concat " " (List.map (fun s -> pr_str true s) xs))) ;

    set "str"
      (function xs ->
         TT.String
           (String.concat "" (List.map (fun s -> pr_str false s) xs))) ;

    set "prn"
      (function xs ->
         print_endline
           (String.concat " " (List.map (fun s -> pr_str true s) xs)) ;
         T.nil) ;

    set "println"
      (function xs ->
         print_endline
           (String.concat " " (List.map (fun s -> pr_str false s) xs)) ;
         T.nil) ;

    set "list"
      (fun xs -> T.list xs) ;

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
        | [TT.List(xs, _)]
        | [TT.Vector(xs, _)] -> T.int (List.length xs)
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

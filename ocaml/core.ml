module T = Types
module E = Env


exception Err of string


let arith_fn f =
  function
  | [ T.Int a ; T.Int b ] -> T.Int(f a b)
  | _ -> raise (Err "Arithmetic functions takes two int arguments.")


let comp_fn f =
  function
  | [ T.Int a ; T.Int b ] -> T.Bool(f a b)
  | _ -> raise (Err "Arithmetic comparisons takes two int arguments.")


let pr_str_list sep readable xs =
  String.concat sep (List.map (Printer.string_of_maltype readable) xs)


let pr_str v =
  T.string (pr_str_list " " true v)

let str v =
  T.string (pr_str_list "" false v)


let prn v =
  print_endline (pr_str_list " " true v) ;
  T.nil

let println v =
  print_endline (pr_str_list " " false v) ;
  T.nil


let read_string =
  function
  | [ T.String s ] -> Reader.read_str s
  | _ -> raise (Err "'read-string' only takes one argument of string.")


let slurp_caml filename =
  let chan = open_in filename in
  let b = Buffer.create 80 in
  Buffer.add_channel b chan (in_channel_length chan) ;
  close_in chan ;
  Buffer.contents b


let slurp =
  function
  | [ T.String s ] -> T.string(slurp_caml s)
  | _ -> raise (Err "'slurp' only takes one argument of string.")


let readline =
  function
  | [T.String x] ->
    print_string x;
    T.string (read_line ())

  | _ ->
    T.string (read_line ())


let nil_q =
  function
  | [T.Nil] -> T.maltrue
  | _ -> T.malfalse


let true_q =
  function
  | [T.Bool true] -> T.maltrue
  | _ -> T.malfalse


let false_q =
  function
  | [T.Bool false] -> T.maltrue
  | _ -> T.malfalse


let number_q =
  function
  | [T.Int _ ] -> T.maltrue
  | _ -> T.malfalse


let string_q =
  function
  | [T.String _] -> T.maltrue
  | _ -> T.malfalse


let keyword_q =
  function
  | [T.Keyword _] -> T.maltrue
  | _ -> T.malfalse


let symbol_q =
  function
  | [T.Symbol _] -> T.maltrue
  | _ -> T.malfalse


let list_q =
  function
  | [ T.List _ ] -> T.maltrue
  | _ -> T.malfalse


let vector_q =
  function
  | [T.Vector _] -> T.maltrue
  | _ -> T.malfalse


let sequential_q =
  function
  | [T.List _]
  | [T.Vector _] -> T.maltrue
  | _ -> T.malfalse


let map_q =
  function
  | [T.Map _] -> T.maltrue
  | _ -> T.malfalse


let fn_q =
  function
  | [ T.Fn _ as fn ] -> T.Bool (not (T.is_macro fn))
  | _ -> T.malfalse


let macro_q =
  function
  | [x] -> T.Bool (T.is_macro x)
  | _ -> raise (Err "'macro?' only takes one argument.")


let atom_q =
  function
  | [ T.Atom _ ] -> T.maltrue
  | _ -> T.malfalse


let keyword =
  function
  | [T.String s] -> T.keyword s
  | _ -> raise (Err "'keyword' only takes one argument of string.")


let symbol =
  function
  | [T.String s] -> T.symbol s
  | _ -> raise (Err "'symbol' only takes one argument of string.")


let atom =
  function
  | [v] -> T.atom v
  | _ -> raise (Err "'atom' only takes one argument.")


let eq =
  function
  | [ a ; b ] -> T.Bool (T.mal_equal a b)
  | _ -> T.malfalse


let meta =
  function
  | [v] -> T.meta v
  | _ -> raise (Err "'meta' only takes one argument.")


let with_meta =
  function
  | [ v ; m ] -> T.with_meta m v
  | _ -> raise (Err "'with-meta' only takes two arguments.")


let deref =
  function
  | [ T.Atom(x) ] -> !x
  | _ -> raise (Err "'deref' only takes one argument of atom.")


let reset_b =
  function
  | [ T.Atom(x) ; v ] -> x := v ; v
  | _ -> raise (Err "'reset!' takes an atom and a value as arguments.")


let swap_b =
  function
  | T.Atom x :: T.Fn(f, _) :: args ->
    let v = f (!x :: args) in
    x := v ;
    v

  | _ ->
    raise (Err "'swap!' takes an atom a function as first two arguments.")


let empty_q =
  function
  | [ T.List([], _) ]
  | [ T.Vector([], _) ] ->
    T.maltrue

  | _ ->
    T.malfalse


let first =
  function
  | [T.Nil] ->
    T.nil

  | [T.List(xs, _)]
  | [T.Vector(xs, _)] ->
    begin try
        List.hd xs
      with Failure _ ->
        T.nil
    end

  | _ ->
    raise (Err "'first' only takes one argument of list/vector/nil.")


let rest =
  function
  | [T.Nil] ->
    T.empty_list

  | [T.List(xs, _)]
  | [T.Vector(xs, _)] ->
    begin try
        T.list (List.tl xs)
      with Failure _ ->
        T.empty_list
    end

  | _ ->
    raise (Err "'rest' only takes one argument of list/vector/nil.")


let count =
  function
  | [ T.List(v, _) ] | [ T.Vector(v, _) ] -> T.int (List.length v)
  | _ -> T.int 0


let nth =
  function
  | [ T.List(xs, _) ; T.Int i ]
  | [ T.Vector(xs, _) ; T.Int i ] ->
    begin match List.nth_opt xs i with
      | Some v -> v
      | None -> raise (Err "Index out of range.")
    end

  | _ ->
    raise (Err "'nth' only takes one argument of list/vector.")


let seq =
  function
  | [T.Nil]
  | [T.String ""]
  | [T.List([], _)]
  | [T.Vector([], _)] ->
    T.nil

  | [T.String s] ->
    T.list
      (*       (s
               |> String.to_seq
               |> List.of_seq
               |> List.map (fun v -> T.string (String.make 1 v))) *)
      (List.init
         (String.length s)
         (fun i -> T.string (String.make 1 (String.get s i))))

  | [ T.List _ as xs ]
  | [ T.Vector _ as xs ] ->
    T.list (T.list_of_container xs)

  | _ ->
    raise (Err "'seq' only takes one argument of list/vector/string/nil.")


let cons =
  function
  | [ x ; T.List(xs, _) ]
  | [ x ; T.Vector(xs, _) ] ->
    T.list (x :: xs)

  | _ ->
    raise (Err "'cons' takes a value and a list/vector as arguments.")


let rec concat = 
  function
  | []
  | [T.List([], _)]
  | [T.Vector([], _)] ->
    T.empty_list

  | [x] as v when T.is_container x ->
    seq v

  | a :: b :: rest when T.is_container a && T.is_container b ->
    concat (T.concat_containers a b :: rest)

  | _ ->
    raise
      (Err "'concat' only takes zero or more list/vector/map as arguments.")


let rec conj =
  function
  | c :: x :: (_ :: _ as xs) ->
    conj ((conj [c; x]) :: xs)

  | [ T.List(v, meta) ; x ] ->
    T.List(x :: v, meta)

  | [ T.Vector(v, meta) ; x ] ->
    T.Vector(v @ [x], meta)

  | [ T.Map(m, meta) ; T.Vector([k ; v], _) ] ->
    T.Map(T.MalMap.add k v m, meta)

  | _ ->
    raise (Err "'conj' takes list/vector/map as first argument.")


let contains_q =
  function
  | [ T.Map(m, _) ; k ] -> T.Bool (T.MalMap.mem k m)
  | _ -> raise (Err "'contains?' takes a map and a value as arguments.")


let get =
  function
  | [ T.Nil ; _ ] ->
    T.nil

  | [ T.Map(m, _) ; k ] ->
    begin match T.MalMap.find_opt k m with
      | Some v -> v
      | None -> T.nil
    end

  | _ ->
    raise (Err "'get' takes a map/nil and a value as arguments.")


let keys =
  function
  | [T.Map(m, _)] -> T.list(T.MalMap.fold (fun k _ ks -> k :: ks) m [])
  | _ -> raise (Err "'keys' only takes one argument of map.")


let vals =
  function
  | [T.Map(m, _)] -> T.list(T.MalMap.fold (fun _ v vs -> v :: vs) m [])
  | _ -> raise (Err "'vals' only takes one argument of map.")


let rec assoc =
  function
  | c :: k :: v :: (_ :: _ as xs) ->
    assoc ((assoc [c; k; v]) :: xs)

  | [T.Nil ; k ; v] ->
    T.map(Types.MalMap.add k v Types.MalMap.empty)

  | [T.Map(m, meta) ; k ; v] ->
    T.Map(Types.MalMap.add k v m, meta)

  | _ ->
    T.nil


let rec dissoc =
  function
  | c :: x :: (_ :: _ as xs) ->
    dissoc ((dissoc [c; x]) :: xs)

  | [ T.Map(m, meta) ; k ] ->
    T.Map(T.MalMap.remove k m, meta)

  | _ ->
    T.nil


let map =
  function
  | [ T.Fn(f, _) ; xs ] ->
    T.list (List.map (fun x -> f [x]) (T.list_of_container xs))

  | _ ->
    raise (Err "'map' takes a function as first argument.")


let apply =
  function
  | T.Fn(f, _) :: apply_args ->
    begin match List.rev apply_args with
      | last_arg :: rev_args ->
        f ((List.rev rev_args) @ (T.list_of_container last_arg))
      | [] -> f []
    end

  | _ ->
    raise (Err "'apply' takes a function as first argument.")


let throw =
  function
  | [ast] -> raise (T.MalExn ast)
  | _ -> raise (Err "'throw' only takes one argument.")


let time_ms =
  function
  | [] -> T.Int (truncate (1000.0 *. Unix.gettimeofday ()))
  | _ -> raise (Err "'time-ms' takes no arguments.")


let init env =
  let set s f =
    E.set s (T.fn f) env in

  (* read print *)

  set "pr-str" pr_str ;
  set "str" str ;
  set "prn" prn ;
  set "println" println ;
  set "read-string" read_string ;
  set "slurp" slurp ;
  set "readline" readline ;

  (* type predicates *)

  set "nil?" nil_q ;
  set "true?" true_q ;
  set "false?" false_q ;
  set "number?" number_q ;
  set "string?" string_q ;
  set "keyword?" keyword_q ;
  set "symbol?" symbol_q ;
  set "list?" list_q ;
  set "vector?" vector_q ;
  set "sequential?" sequential_q ;
  set "map?" map_q ;
  set "fn?" fn_q ;
  set "macro?" macro_q ;
  set "atom?" atom_q ;

  (* value constructors *)

  set "keyword" keyword ;
  set "symbol" symbol ;
  set "list" T.list ;
  set "vector" T.vector ;
  set "hash-map" T.map_of_list ;
  set "atom" atom ;

  (* equality *)

  set "=" eq ;

  (* arithmentic operations and comparisons *)

  set "+" (arith_fn ( + )) ;
  set "-" (arith_fn ( - )) ;
  set "*" (arith_fn ( * )) ;
  set "/" (arith_fn ( / )) ;

  set "<"  (comp_fn ( < )) ;
  set "<=" (comp_fn ( <= )) ;
  set ">"  (comp_fn ( > )) ;
  set ">=" (comp_fn ( >= )) ;

  (* metadata *)

  set "meta" meta ;
  set "with-meta" with_meta ;

  (* atom operations *)

  set "deref" deref ;
  set "reset!" reset_b ;
  set "swap!" swap_b ;

  (* sequence operations *)

  set "empty?" empty_q ;
  set "first" first ;
  set "rest" rest ;
  set "count" count ;
  set "nth" nth ;
  set "seq" seq ;
  set "cons" cons ;
  set "concat" concat ;
  set "conj" conj ;

  (* map operations *)

  set "assoc" assoc ;
  set "dissoc" dissoc ;
  set "get" get ;
  set "contains?" contains_q ;
  set "keys" keys ;
  set "vals" vals ;

  (* functional stuffs *)

  set "map" map ;
  set "apply" apply ;

  (* throw exception *)

  set "throw" throw ;

  (* misc *)

  set "time-ms" time_ms ;

  env

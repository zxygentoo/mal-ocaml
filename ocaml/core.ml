module T = Types
module TT = Types.Types
module E = Env


exception Err of string


let arith_fn f =
  function
  | [ TT.Int a ; TT.Int b ] ->
    TT.Int(f a b)

  | _ ->
    raise (Err "Arithmetic functions require two int arguments.")


let comp_fn f =
  function
  | [ TT.Int a ; TT.Int b ] ->
    TT.Bool(f a b)

  | _ ->
    raise (Err "Arithmetic comparisons require two int arguments.")


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
  | [ TT.String s ] ->
    Reader.read_str s

  | _ ->
    raise (Err "Invalid argument for read-string")


let slurp_caml filename =
  let chan = open_in filename in
  let b = Buffer.create 80 in
  Buffer.add_channel b chan (in_channel_length chan) ;
  close_in chan ;
  Buffer.contents b


let slurp =
  function
  | [ TT.String s ] ->
    T.string(slurp_caml s)

  | _ ->
    raise (Err "Invalid arguments for 'slurp'.")


let list_q =
  function
  | [ TT.List _ ] ->
    T.maltrue

  | _ ->
    T.malfalse


let atom =
  function
  | [v] ->
    T.atom v

  | _ ->
    raise (Err "Invalid arguments for 'atom'.")


let atom_q =
  function
  | [ TT.Atom _ ] ->
    T.maltrue

  | _ ->
    T.malfalse


let deref =
  function
  | [ TT.Atom(x) ] ->
    !x

  | _ ->
    raise (Err "Invalid arguments for 'deref'.")


let reset_b =
  function
  | [ TT.Atom(x) ; v ] ->
    x := v ;
    v

  | _ ->
    raise (Err "Invalid arguments for 'reset!'.")


let swap_b =
  function
  | TT.Atom x :: TT.Fn(f, _) :: args ->
    let v = f (!x :: args) in
    x := v ;
    v

  | _ ->
    raise (Err "Invalid arguments for 'swap!'.")


let eq =
  function
  | [ a ; b ] ->
    TT.Bool (T.mal_equal a b)

  | _ ->
    T.malfalse


let empty_q =function
  | [ TT.List([], _) ]

  | [ TT.Vector([], _) ] ->
    T.maltrue

  | _ ->
    T.malfalse


let count =
  function
  | [ TT.List(v, _) ]

  | [ TT.Vector(v, _) ] ->
    T.int (List.length v)

  | _ ->
    T.int 0


let cons =
  function
  | [ x ; TT.List(xs, _) ]

  | [ x ; TT.Vector(xs, _) ] ->
    T.list (x :: xs)

  | _ ->
    raise (Err "Invalid argument for 'cons'.")


let seq =
  function
  | [TT.Nil]

  | [TT.String ""]

  | [TT.List([], _)]

  | [TT.Vector([], _)] ->
    T.nil

  | [TT.String s] ->
    T.list
      (s
       |> String.to_seq
       |> List.of_seq
       |> List.map (fun v -> T.string (String.make 1 v)))

  | [ TT.List _ as xs ]

  | [ TT.Vector _ as xs ] ->
    T.list (T.list_of_container xs)

  | _ ->
    raise (Err "Invalid argument for 'seq'.")


let rec concat = 
  function
  | []

  | [TT.List([], _)]

  | [TT.Vector([], _)] ->
    T.empty_list

  | [x] as v when T.is_container x ->
    seq v

  | a :: b :: rest when T.is_container a && T.is_container b ->
    concat (T.concat_containers a b :: rest)

  | _ ->
    raise (Err "Invalid argument for 'concat'.")


let meta =
  function
  | [v] ->
    T.meta v

  | _ ->
    raise (Err "Invalid argument for 'meta'.")


let with_meta =
  function
  | [ v ; m ] ->
    T.with_meta v m

  | _ ->
    raise (Err "Invalid argument for 'with-meta'.")


let macro_q =
  function
  | [x] ->
    TT.Bool (T.is_macro x)

  | _ ->
    raise (Err "Invalid argument for 'macro?.")


let nth =
  function
  | [ TT.List(xs, _) ; TT.Int i ]
  | [ TT.Vector(xs, _) ; TT.Int i ] ->
    begin match List.nth_opt xs i with
      | Some v ->
        v

      | None ->
        raise (Err "Index out of range.")
    end

  | _ ->
    raise (Err "Invalid argument for 'nth'.")


let first =
  function
  | [TT.Nil] ->
    T.nil

  | [TT.List(xs, _)]

  | [TT.Vector(xs, _)] ->
    begin try
        List.hd xs
      with Failure _ ->
        T.nil
    end

  | _ ->
    raise (Err "Invalid argument for 'first'.")


let rest =
  function
  | [TT.Nil] ->
    T.empty_list

  | [TT.List(xs, _)]

  | [TT.Vector(xs, _)] ->
    begin try
        T.list (List.tl xs)
      with Failure _ ->
        T.empty_list
    end

  | _ ->
    raise (Err "Invalid argument for 'rest'.")

let throw =
  function
  | [ast] -> raise (T.MalExn ast)

  | _ ->
    raise (Err "Invalid argument for 'throw'.")


let apply =
  function
  | TT.Fn(f, _) :: apply_args ->
    begin match List.rev apply_args with
      | last_arg :: rev_args ->
        f ((List.rev rev_args) @ (T.list_of_container last_arg))
      | [] -> f []
    end
  | _ ->
    raise (Invalid_argument "First arg to apply must be a fn")


let map =
  function
  | [ TT.Fn(f, _) ; xs ] ->
    T.list (List.map (fun x -> f [x]) (T.list_of_container xs))

  | _ ->
    raise (Invalid_argument "First arg to apply must be a fn")


let nil_q =
  function
  | [TT.Nil] ->
    T.maltrue

  | _ ->
    T.malfalse


let true_q =
  function
  | [TT.Bool true] ->
    T.maltrue

  | _ ->
    T.malfalse


let false_q =
  function
  | [TT.Bool false] ->
    T.maltrue

  | _ ->
    T.malfalse


let number_q =
  function
  | [TT.Int _ ] ->
    T.maltrue

  | _ ->
    T.malfalse


let string_q =
  function
  | [TT.String _] ->
    T.maltrue

  | _ ->
    T.malfalse


let symbol_q =
  function
  | [TT.Symbol _] ->
    T.maltrue

  | _ ->
    T.malfalse


let keyword_q =
  function
  | [TT.Keyword _] ->
    T.maltrue

  | _ ->
    T.malfalse


let vector_q =
  function
  | [TT.Vector _] ->
    T.maltrue

  | _ ->
    T.malfalse


let sequential_q =
  function
  | [TT.List _]

  | [TT.Vector _] ->
    T.maltrue

  | _ ->
    T.malfalse


let map_q =
  function
  | [TT.Map _] ->
    T.maltrue

  | _ ->
    T.malfalse


let fn_q =
  function
  | [ TT.Fn _ as fn ] ->
    TT.Bool (not (T.is_macro fn))

  | _ ->
    T.malfalse


let symbol =
  function
  | [TT.String s] ->
    T.symbol s

  | _ ->
    raise (Err "Invalid argument for 'symbol'.")


let keyword =
  function
  | [TT.String s] ->
    T.keyword s

  | _ ->
    raise (Err "Invalid argument for 'keyword'.")


let rec assoc =
  function
  | c :: k :: v :: (_ :: _ as xs) ->
    assoc ((assoc [c; k; v]) :: xs)

  | [TT.Nil ; k ; v] ->
    T.map(Types.MalMap.add k v Types.MalMap.empty)

  | [TT.Map(m, meta) ; k ; v] ->
    TT.Map(Types.MalMap.add k v m, meta)
  | _ ->
    T.nil


let rec dissoc =
  function
  | c :: x :: (_ :: _ as xs) ->
    dissoc ((dissoc [c; x]) :: xs)

  | [ TT.Map(m, meta) ; k ] ->
    TT.Map(T.MalMap.remove k m, meta)

  | _ ->
    T.nil


let get =
  function
  | [ TT.Nil ; _ ] ->
    T.nil

  | [ TT.Map(m, _) ; k ] ->
    begin match T.MalMap.find_opt k m with
      | Some v ->
        v

      | None ->
        T.nil
    end

  | _ ->
    raise (Err "Invalid argument for 'get'.")


let contains_q =
  function
  | [ TT.Map(m, _) ; k ] ->
    TT.Bool (T.MalMap.mem k m)

  | _ ->
    raise (Err "Invalid argument for 'contains?'.")


let keys =
  function
  | [TT.Map(m, _)] ->
    T.list(T.MalMap.fold (fun k _ ks -> k :: ks) m [])

  | _ ->
    raise (Err "Invalid argument for 'keys'.")


let vals =
  function
  | [TT.Map(m, _)] ->
    T.list(T.MalMap.fold (fun _ v vs -> v :: vs) m [])

  | _ ->
    raise (Err "Invalid argument for 'vals'.")


let readline =
  function
  | [TT.String x] ->
    print_string x;
    T.string (read_line ())

  | _ ->
    T.string (read_line ())


let time_ms =
  function
  | [] ->
    TT.Int (truncate (1000.0 *. Unix.gettimeofday ()))

  | _ ->
    raise (Err "Invalid argument for 'time-ms'.")


let rec conj =
  function
  | c :: x :: (_ :: _ as xs) ->
    conj ((conj [c; x]) :: xs)

  | [ TT.Map(m, meta) ; TT.Vector([k ; v], _) ]
    -> TT.Map(T.MalMap.add k v m, meta)

  | [ TT.List(v, meta) ; x ]
    -> TT.List(x :: v, meta)

  | [ TT.Vector(v, meta) ; x ]
    -> TT.Vector(v @ [x], meta)

  | _ ->
    T.nil


let init env =
  let set s f =
    E.set s (T.fn f) env in

  set "+" (arith_fn ( + )) ;
  set "-" (arith_fn ( - )) ;
  set "*" (arith_fn ( * )) ;
  set "/" (arith_fn ( / )) ;

  set "<"  (comp_fn ( < )) ;
  set "<=" (comp_fn ( <= )) ;
  set ">"  (comp_fn ( > )) ;
  set ">=" (comp_fn ( >= )) ;

  set "pr-str" pr_str ;
  set "str" str ;
  set "prn" prn ;
  set "println" println ;
  set "read-string" read_string ;
  set "slurp" slurp ;

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

  set "=" eq ;

  set "keyword" keyword ;
  set "symbol" symbol ;
  set "list" T.list ;
  set "vector" T.vector ;
  set "hash-map" T.map_of_list ;
  set "atom" atom ;

  set "meta" meta ;
  set "with-meta" with_meta ;

  set "deref" deref ;
  set "reset!" reset_b ;
  set "swap!" swap_b ;

  set "empty?" empty_q ;

  set "first" first ;
  set "rest" rest ;
  set "count" count ;
  set "nth" nth ;

  set "cons" cons ;
  set "concat" concat ;

  set "seq" seq ;

  set "throw" throw ;

  set "assoc" assoc ;
  set "dissoc" dissoc ;
  set "get" get ;
  set "contains?" contains_q ;
  set "keys" keys ;
  set "vals" vals ;

  set "apply" apply ;
  set "map" map ;

  set "readline" readline ;
  set "time-ms" time_ms ;
  set "conj" conj ;
  env

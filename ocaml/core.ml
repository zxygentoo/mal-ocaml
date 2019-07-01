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
  | [x] ->
    T.list (T.list_of_container x)
  | _ ->
    raise (Err "Invalid argument for 'seq'.")


let rec concat = 
  function
  | [] ->
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

  set "list" T.list ;
  set "list?" list_q ;
  set "atom" atom ;
  set "atom?" atom_q ;

  set "deref" deref ;
  set "reset!" reset_b ;
  set "swap!" swap_b ;

  set "=" eq ;
  set "empty?" empty_q ;
  set "count" count ;

  set "cons" cons ;
  set "concat" concat ;

  set "seq" seq ;

  set "meta" meta ;
  set "with-meta" with_meta ;

  set "macro?" macro_q ;

  set "nth" nth ;
  set "first" first ;
  set "rest" rest ;

  env

module T = Types.Types


let rec string_of_maltype print_readably exp =
  let r = print_readably in
  match exp with
  | T.Nil ->
    "nil"

  | T.Bool(true) ->
    "true"

  | T.Bool(false) ->
    "false"

  | T.Int(i) ->
    string_of_int i

  | T.String(s) ->
    if r then "\"" ^ (String.escaped s) ^ "\"" else s

  | T.Keyword(kw) ->
    ":" ^ kw

  | T.Symbol(s, _) ->
    s

  | T.List(v, _) ->
    string_of_list_or_vector r v "(" ")"

  | T.Vector(v, _) ->
    string_of_list_or_vector r v "[" "]"

  | T.Map(v, _) ->
    "{" ^ 
    (Types.MalMap.fold
       (fun k v s ->
          s ^ (if s = "" then "" else " ") ^
          (string_of_maltype r k) ^ " " ^ (string_of_maltype r v))
       v
       "") ^ "}"

  | T.Fn(_, _) ->
    "#<function>"

  | T.Atom(x) ->
    "(atom " ^ (string_of_maltype r !x) ^ ")"


and string_of_list_or_vector r v sol eol=
  sol ^ (String.concat " " (List.map (fun s -> string_of_maltype r s) v)) ^ eol


(* api *)

let print_str exp =
  exp |> string_of_maltype true |> print_endline

module T = Types.Types

let rec string_of_maltype exp =
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
    "\"" ^ s ^ "\""

  | T.Keyword(kw) ->
    ":" ^ kw

  | T.Symbol(s, _) ->
    s

  | T.List(xs, _) ->
    string_of_list_or_vector xs "(" ")"

  | T.Vector(xs, _) ->
    string_of_list_or_vector xs "[" "]"

  | T.Map(xs, _) ->
    "{" ^ 
    (Types.MalMap.fold
       (fun k v s ->
          s ^
          (if s = "" then "" else ", ") ^
          (string_of_maltype k) ^ " " ^ (string_of_maltype v))
       xs 
       ""
    ) ^
    "}"

  | T.Fn(_, _) ->
    "#[function]"

  | T.Atom(x) ->
    "(atom " ^ (string_of_maltype !x) ^ ")"

and string_of_list_or_vector xs sol eol=
  sol ^
  (String.concat " " (List.map (fun s -> string_of_maltype s) xs)) ^
  eol

(* api *)

let print_str exp =
  exp |> string_of_maltype |> print_endline

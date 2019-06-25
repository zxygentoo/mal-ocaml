module T = Types.Types

let rec string_of_exp exp =
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
          (string_of_exp k) ^ " " ^ (string_of_exp v))
       xs 
       ""
    ) ^
    "}"

  | T.Fn(_, _) ->
    "#[fn] not yet printable ..."

  | T.Atom(_) ->
    "#[atom] not yet printable ..."

and string_of_list_or_vector xs sol eol=
  sol ^
  (String.concat " " (List.map (fun s -> string_of_exp s) xs)) ^
  eol

(* api *)

let print_str exp =
  exp |> string_of_exp |> print_endline

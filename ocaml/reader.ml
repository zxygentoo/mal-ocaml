module T = Types


exception Err of string
exception Nothing


(* regex patterns *)

let int_re = Str.regexp "-?[0-9]+$"
let token_re = Str.regexp "~@\\|[][{}()'`~^@]\\|\"\\(\\\\.\\|[^\"]\\)*\"?\\|;.*\\|[^][  \n{}('\"`,;)]*"


(* helpers *)

let startswith s sol =
  if String.length s = 0 then false else s.[0] = sol


let is_comment s =
  startswith s ';'


let is_string_literal s =
  startswith s '"'


let is_keyword_literal s =
  startswith s ':'


let is_int_literal s =
  Str.string_match int_re s 0


(* tokenization *)

let tokenize s =
  let filter_delims results =
    List.filter
      (function
        | Str.Delim _ -> true
        | Str.Text _ -> false)
      results in

  let delims_to_strs results =
    List.map
      (function
        | Str.Delim x -> x
        | Str.Text _ -> raise (Err "Tokenization error."))
      results in

  s
  |> Str.full_split token_re
  |> filter_delims
  |> delims_to_strs
  |> List.filter (( <> ) "")


(* read functions *)

let rec read_form =
  function
  | [] ->
    raise Nothing

  | "" :: xs ->
    read_form xs

  | x :: xs when is_comment x ->
    read_form xs

  | "(" :: xs ->
    read_list xs

  | "[" :: xs ->
    read_vector xs

  | "{" :: xs ->
    read_map xs

  | "'"  :: xs ->
    read_quote "quote" xs

  | "`"  :: xs ->
    read_quote "quasiquote" xs

  | "~"  :: xs ->
    read_quote "unquote" xs

  | "~@" :: xs ->
    read_quote "splice-unquote" xs

  | "@"  :: xs ->
    read_atom xs

  | "^" :: xs ->
    read_with_meta xs

  | x :: xs ->
    (read_salar x, xs)

and read_list tokens =
  let forms, tokens_left = read_container ")" [] tokens in
  (T.list forms, tokens_left)

and read_vector tokens =
  let forms, tokens_left = read_container "]" [] tokens in
  (T.vector forms, tokens_left)

and read_map tokens =
  let forms, tokens_left = read_container "}" [] tokens in
  try
    (T.map_of_list forms, tokens_left)
  with T.Err msg ->
    raise (Err msg)

and read_container eol forms tokens =
  match tokens with
  | [] ->
    raise (Err "Can't read unbalanced form.")

  | x :: xs when x = eol ->
    (forms, xs)

  | x :: xs when is_comment x ->
    read_container eol forms xs

  | _ ->
    let form, tokens_left = read_form tokens in
    read_container eol (forms @ [form]) tokens_left

and read_quote sym tokens =
  let form, tokens_left = read_form tokens in
  (T.list [T.symbol sym; form], tokens_left)

and read_atom tokens =
  read_quote "deref" tokens

and read_with_meta tokens =
  let meta, tokens_left_meta = read_form tokens in
  let value, tokens_left = read_form tokens_left_meta in
  (Types.list [Types.symbol "with-meta"; value; meta], tokens_left)

and read_salar =
  function
  | "nil" ->
    T.nil

  | "true" ->
    T.maltrue

  | "false" ->
    T.malfalse

  | x when is_int_literal x ->
    T.int (int_of_string x)

  | x when is_string_literal x ->
    let len = String.length x in
    if x.[len - 1] <> '"' then
      raise (Err "Unexpected end of input.")
    else
      T.string (Scanf.unescaped (String.sub x 1 (len - 2)))

  | x when is_keyword_literal x ->
    T.keyword (String.sub x 1 (String.length x - 1))

  | sym ->
    T.symbol sym


(* api *)

let read_str s =
  let form, _ = read_form (tokenize s) in
  form

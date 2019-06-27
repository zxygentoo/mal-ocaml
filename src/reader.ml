module T = Types

exception Err of string
exception Nothing

(* helpers *)

let gsub re f str =
  String.concat
    "" (List.map
          (function
            | Str.Delim x -> f x
            | Str.Text x -> x)
          (Str.full_split re str))


(* tokenization *)

let token_re =
  Str.regexp
    "~@\\|[][{}()'`~^@]\\|\"\\(\\\\.\\|[^\"]\\)*\"?\\|;.*\\|[^][  \n{}('\"`,;)]*"

let tokenize s =
  let split s =
    Str.full_split token_re s in

  let seg rs =
    List.filter
      (function | Str.Delim _ -> true
                | Str.Text _ -> false)
      rs in

  let to_str rs =
    List.map
      (function | Str.Delim x -> x
                | Str.Text _ -> raise (Err "Tokenization error."))
      rs in

  let filter_empty_str ss =
    List.filter ((<>) "") ss in

  s |> split |> seg |> to_str |> filter_empty_str

(* read functions *)

let rec read_form tokens =
  match tokens with
  | [] ->
    raise Nothing

  | x :: xs -> begin
      match x with
      | "(" ->
        read_list xs

      | "[" ->
        read_vector xs

      | "{" ->
        read_map xs

      | "'"  ->
        read_quote "quote" xs

      | "`"  ->
        read_quote "quasiquote" xs

      | "~"  ->
        read_quote "unquote" xs

      | "~@" ->
        read_quote "splice-unquote" xs

      | "@"  ->
        read_atom xs

      | "^" ->
        read_with_meta xs

      | _ ->
        (read_salar x, xs)
    end

and read_list tokens =
  let forms, tokens_left = read_container ")" [] tokens in
  (T.list forms, tokens_left)

and read_vector tokens =
  let forms, tokens_left = read_container "]" [] tokens in
  (T.vector forms, tokens_left)

and read_map tokens =
  let forms, tokens_left = read_container "}" [] tokens in
  begin
    try
      (T.map_of_list forms, tokens_left)
    with Invalid_argument s ->
      raise (Err s)
  end

and read_container eol forms tokens =
  match tokens with
  | [] ->
    raise (Err "Unbanlanced input.")

  | x :: xs when x = eol ->
    (forms, xs)

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

and read_salar token =
  try
    T.int (int_of_string token)
  with
  | Failure _ -> begin
      match token with
      | "nil" ->
        T.nil

      | "true" ->
        T.maltrue

      | "false" ->
        T.malfalse

      | _ -> begin
          let len = String.length token in
          match List.init len (String.get token) with
          | [] ->
            raise (Err "Unexpected end of input.")

          | x :: _ when x = '"' ->
            if token.[len - 1] = '"'
            then T.string (gsub
                             (Str.regexp "\\\\.")
                             (function
                               | "\\n" -> "\n"
                               | x -> String.sub x 1 1)
                             (String.sub token 1 ((String.length token) - 2)))
            else raise (Err "Unexpected end of string literal.")

          | x :: _ when x = ':' ->
            T.keyword (String.sub token 1 (len - 1))

          | _ ->
            T.symbol token
        end
    end


(* api *)

let read_str s =
  let form, _ = read_form (tokenize s) in
  form

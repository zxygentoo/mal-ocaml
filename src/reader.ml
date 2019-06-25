module T = Types

exception ReaderErr of string
exception Nothing

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
                | Str.Text _ -> raise (ReaderErr "Tokenization error!")) 
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
        let forms, tokens_left = read_sequnence ")" [] xs in
        (T.list forms, tokens_left)

      | "[" ->
        let forms, tokens_left = read_sequnence "]" [] xs in
        (T.vector forms, tokens_left)

      | "{" ->
        let forms, tokens_left = read_sequnence "}" [] xs in
        begin
          try
            (T.map_of_list forms, tokens_left)
          with Invalid_argument s ->
            raise (ReaderErr s)
        end

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

and read_sequnence eol forms tokens =
  match tokens with
  | [] ->
    raise (ReaderErr "Unexpected end of string!")

  | x :: xs when x = eol ->
    (forms, xs)

  | _ ->
    let form, tokens_left = read_form tokens in
    read_sequnence eol (forms @ [form]) tokens_left

and read_quote sym tokens =
  let form, tks = read_form tokens in
  (T.list [T.symbol sym; form], tks)

and read_atom tokens =
  read_quote "deref" tokens

and read_with_meta tokens =
  let meta, tokens_left_meta = read_form tokens in
  let value, tokens_left = read_form tokens_left_meta in
  ( Types.list [Types.symbol "with-meta"; value; meta]
  , tokens_left)

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
            raise (ReaderErr "Unexpected end of string!")

          | x :: _ when x = '"' ->
            if String.get token (len - 1) <> '"'
            then raise (ReaderErr "Unexpected end of string!")
            else T.string (String.sub token 1 (len - 2))

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

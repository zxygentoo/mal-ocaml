module rec Types
  : sig
    type t =
      | Nil
      | Bool of bool
      | Int of int
      | String of string
      | Keyword of string
      | Symbol of string * t
      | List of t list * t
      | Vector of t list * t
      | Map of t MalMap.t * t
      | Fn of (t list -> t) * t
      | Atom of t ref
  end = Types

and MalValue
  : sig
    type t = Types.t
    val compare : t -> t -> int
  end = 
struct
  type t = Types.t
  let compare = Pervasives.compare
end

and MalMap
  : Map.S with type key = MalValue.t
  = Map.Make(MalValue)


type maltype = MalValue.t


exception Err of string

exception MalExn of Types.t


let nil = Types.Nil
let maltrue = Types.Bool true
let malfalse = Types.Bool false


let int x =
  Types.Int x


let string x =
  Types.String x


let keyword x =
  Types.Keyword x


let symbol x = 
  Types.Symbol(x, nil)


let list x = 
  Types.List(x, nil)


let empty_list = list []


let vector x = 
  Types.Vector(x, nil)


(* let empty_vector = vector [] *)


let map x =
  Types.Map(x, nil)


let map_of_list x =
  let rec aux acc x =
    match x with
    | [] ->
      map acc

    | k :: v :: xs ->
      aux (MalMap.add k v acc) xs

    | _ :: [] ->
      raise (Err "Map must contain even number of elements.")
  in
  aux MalMap.empty x


let fn x = 
  Types.Fn(x, nil)


let atom x =
  Types.Atom (ref x)


let is_container =
  function
  | Types.List _

  | Types.Vector _

  | Types.Map _ ->
    true

  | _ ->
    false


let to_bool =
  function
  | Types.Nil
  | Types.Bool(false) -> false
  | _ -> true


let list_of_container =
  function
  | Types.List(xs, _)

  | Types.Vector(xs, _) ->
    xs

  | Types.Map(xs, _) ->
    MalMap.fold (fun k v m -> vector [ k ; v ] :: m) xs [] |> List.rev

  | _ ->
    raise (Err "Invalid argument for 'list_of_container': not a list/vector/map.")


let concat_containers a b =
  if is_container a && is_container b then
    list((list_of_container a) @ (list_of_container b))
  else
    raise (Err "Invalid argument for '( @ )': can only concat two sequnences.")


let meta =
  function
  | Types.Symbol(_, meta)

  | Types.List(_, meta)

  | Types.Vector(_, meta)

  | Types.Map(_, meta)

  | Types.Fn(_, meta) ->
    meta

  | _ ->
    raise (Err "Metadata not supported on this type.")


(* Mal guides doesn't specify these details, so we do what Clojure does. *)
let parse_meta =
  function
  | Types.Symbol(s, _)

  | Types.String s ->
    map_of_list [ keyword "tag" ; string s ]

  | Types.Keyword kw ->
    map_of_list [ keyword kw ; maltrue ]

  | Types.Map(m, _) ->
    map m

  | _ ->
    raise (Err "Metadata must be a symbol, keyword, string or map.")


let with_meta value meta =
  match value with

  | Types.Symbol(x, _) ->
    Types.Symbol(x, meta)

  | Types.List(x, _) ->
    Types.List(x, meta)

  | Types.Vector(x, _) ->
    Types.Vector(x, meta)

  | Types.Map(x, _) ->
    Types.Map(x, meta)

  | Types.Fn(x, _) ->
    Types.Fn(x, meta)

  | _ ->
    raise (Err "Metadata not supported on this type.")


let macro_kw = keyword "macro"


let set_macro =
  function
  | Types.Fn(x, meta) ->
    begin match meta with
      | Map(m, _) ->
        Types.Fn(x, map(MalMap.add macro_kw maltrue m))

      | Types.Nil ->
        Types.Fn(x, map(MalMap.add macro_kw maltrue MalMap.empty))

      | _ ->
        raise (Err "Can't set macro, invalid metadata.")
    end
  | _ ->
    raise (Err "Macro must be a function.")


let is_macro =
  function
  | Types.Fn(_, Types.Map(meta, _)) ->
    MalMap.find_opt macro_kw meta = Some maltrue

  | _ ->
    false


let can_be_meta =
  function
  | Types.Symbol _

  | Types.String _

  | Types.Keyword _

  | Types.Map _ ->
    true

  | _ ->
    false


let rec mal_equal a b =
  match a, b with
  | Types.List(x, _), Types.List(y, _)
  | Types.List(x, _), Types.Vector(y, _)
  | Types.Vector(x, _), Types.Vector(y, _)
  | Types.Vector(x, _), Types.List(y, _) ->
    mal_sequence_equal x y

  | Types.Map(x, _), Types.Map(y, _) ->
    mal_map_equal x y

  | _ ->
    a = b

and mal_sequence_equal a b =
  List.length a = List.length b
  && List.for_all2 mal_equal a b

and mal_map_equal a b =
  MalMap.cardinal a = MalMap.cardinal b
  &&
  MalMap.for_all
    (fun k va ->
       MalMap.mem k b
       &&
       mal_equal va (MalMap.find k b))
    a

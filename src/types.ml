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


let vector x = 
  Types.Vector(x, nil)


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


let to_bool =
  function
  | Types.Nil
  | Types.Bool(false) -> false
  | _ -> true


let is_list =
  function
  | Types.List(_, _) -> true
  | _ -> false


let rec mal_equal a b =
  match a, b with
  | Types.List(x, _), Types.List(y, _)
  | Types.List(x, _), Types.Vector(y, _)
  | Types.Vector(x, _), Types.Vector(y, _)
  | Types.Vector(x, _), Types.List(y, _) ->
    mal_sequnce_equal x y

  | Types.Map(x, _), Types.Map(y, _) ->
    mal_map_equal x y

  | _ ->
    a = b

and mal_sequnce_equal a b =
  List.length a = List.length b
  && List.for_all2 mal_equal a b

and mal_map_equal a b =
  let identical_to_b k v =
    MalMap.mem k b && mal_equal v (MalMap.find k b)
  in
  MalMap.cardinal a = MalMap.cardinal b
  && MalMap.for_all identical_to_b a

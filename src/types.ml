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
  end
= struct
  type t = Types.t
  let compare = Pervasives.compare
end

and MalMap
  : Map.S with type key = MalValue.t
  = Map.Make(MalValue)

type maltype = MalValue.t

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
      raise (Invalid_argument "Map must contain even elements.")
  in
  aux MalMap.empty x

let fn x = 
  Types.Fn(x, nil)

let is_falsey = function
  | Types.Nil | Types.Bool(true) -> true
  | _ -> false

(* This type definition is a bit awkward.

   This some what complex thing is the combination of
   the way OCaml use functor to construct Map module and the fact
   module is kind of second-class in the language.

   Sure we can use a map from string to malvalue_type to models this,
   as a matter of fact some implementations in the mal repo do, or maybe
   use some other tricks. but that's kinda ugly too.

   Current definition will expose two modules for types:
   Types for the MalMap and general type related functions,
   and Types.Types for actual value constructors.

   This isn't the prettiest, but still manageable. *)

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


exception Err of string

exception MalExn of Types.t


(* value construction shortcuts *)

let nil = Types.Nil

let maltrue  = Types.Bool true
let malfalse = Types.Bool false

let int x     = Types.Int x
let string x  = Types.String x
let keyword x = Types.Keyword x
let symbol x  = Types.Symbol(x, nil)
let list x    = Types.List(x, nil)
let vector x  = Types.Vector(x, nil)
let map x     = Types.Map(x, nil)
let fn x      = Types.Fn(x, nil)
let atom x    = Types.Atom (ref x)

let empty_list = list []


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


(* truthiness *)

let to_bool =
  function
  | Types.Nil | Types.Bool(false) -> false
  | _ -> true


(* container type (list/vector/map) helpers *)

let is_container =
  function
  | Types.List _ | Types.Vector _ | Types.Map _ -> true
  | _ -> false


let list_of_container =
  function
  | Types.List(xs, _) | Types.Vector(xs, _) ->
    xs

  | Types.Map(xs, _) ->
    MalMap.fold (fun k v m -> vector [ k ; v ] :: m) xs [] |> List.rev

  | _ ->
    raise (Err "Not a container type.")


let concat_containers a b =
  if is_container a && is_container b then
    list((list_of_container a) @ (list_of_container b))
  else
    raise (Err "Can only concat two conatiner types.")


(* meta *)

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


let with_meta meta =
  function
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


(* macro *)

let macro_kw = keyword "macro"


(* This is not really `set`, we just create a new mal function
   with the same native ocaml function with different metadata. *)
let set_macro =
  function
  | Types.Fn(x, meta) ->
    begin match meta with
      | Map(m, _) ->
        Types.Fn(x, map(MalMap.add macro_kw maltrue m))

      | Types.Nil ->
        Types.Fn(x, map(MalMap.add macro_kw maltrue MalMap.empty))

      | _ ->
        (* If meta of fn is not nil/map, this will fail.
           Clojure requires meta to be a map, Mal doesn't.
           But this doesn't effect passing tests and self-hosting. *)
        raise (Err "Meta must be a map/nil for set as macro.")
    end

  | _ ->
    raise (Err "Macro must be a function.")


let is_macro =
  function
  | Types.Fn(_, Types.Map(meta, _)) ->
    MalMap.find_opt macro_kw meta = Some maltrue

  | _ ->
    false


(* equality *)

let rec mal_equal a b =
  match a, b with
  | Types.List(x, _),   Types.List(y, _)
  | Types.List(x, _),   Types.Vector(y, _)
  | Types.Vector(x, _), Types.Vector(y, _)
  | Types.Vector(x, _), Types.List(y, _) ->
    mal_sequence_equal x y

  | Types.Map(x, _), Types.Map(y, _) ->
    mal_map_equal x y

  | _ ->
    a = b

and mal_sequence_equal a b =
  List.length a = List.length b && List.for_all2 mal_equal a b

and mal_map_equal a b =
  MalMap.cardinal a = MalMap.cardinal b
  &&
  MalMap.for_all
    (fun k va -> MalMap.mem k b && mal_equal va (MalMap.find k b))
    a

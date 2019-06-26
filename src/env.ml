module T = Types
module M = Map.Make(String)


type env = { current : T.maltype M.t ref
           ; outer : env option }


exception EnvErr of string


let create outer =
  { current = ref M.empty
  ; outer = outer }


let root () =
  create None


let set k v env =
  env.current := M.add k v !(env.current)


let rec get k env =
  match M.find_opt k !(env.current) with
  | Some(_) as v ->
    v

  | None -> begin match env.outer with
      | None ->
        None

      | Some(outer) ->
        get k outer
    end

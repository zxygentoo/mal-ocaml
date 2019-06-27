module T = Types
module M = Map.Make(String)


type env = { current : T.maltype M.t ref
           ; outer : env option }


exception EnvErr of string


let make outer =
  { current = ref M.empty
  ; outer = outer }


let root () =
  make None


let set k v env =
  env.current := M.add k v !(env.current)


let rec find k env =
  match M.find_opt k !(env.current) with
  | Some(_) ->
    Some(env)

  | None -> begin match env.outer with
      | None ->
        None

      | Some(outer) ->
        find k outer
    end

let get k env =
  match find k env with
  | Some(env) ->
    M.find_opt k !(env.current)

  | None ->
    None

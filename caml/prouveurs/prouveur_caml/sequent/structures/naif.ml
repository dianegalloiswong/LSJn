let rec rm x = function
  | [] -> assert false
  | h::t when h=x -> t
  | h::t -> h::(rm x t)

module G = 
struct
  type t = (int*int) list ref
  let empty () = ref []
  let add c t = t := c:: !t
  let rm c t = t := rm c !t
  let fold_vars _ = assert false
  let priorite_plus_forte n t =
    List.fold_left (fun (p,c) (i,sf) ->
      if i<=n && !Global_ref.priorite.(sf)<p then
	!Global_ref.priorite.(sf),(i,sf)
      else
	p,c
    ) (6,(0,0)) !t
  let all_imp n t =
    List.filter (fun (i,sf) -> i<=n && !Global_ref.priorite.(sf)=5) !t
  let nombre_imp n t = List.length (all_imp n t)
  let nth_imp n k t = List.nth (List.sort compare (all_imp n t)) k
  let reord_imp n k t = ()
  let print t = assert false
end

module D = 
struct
  type t = (int*int) list ref
  let empty () = ref []
  let add c t = t := c:: !t
  let rm c t = t := rm c !t
  let fold_vars _ = assert false
  let priorite_plus_forte n t =
    List.fold_left (fun (p,c) (i,sf) ->
      if i=n && !Global_ref.priorite.(sf)<p then
	!Global_ref.priorite.(sf),(i,sf)
      else
	p,c
    ) (6,(0,0)) !t
  let all_imp n t =
    List.filter (fun (i,sf) -> i=n && !Global_ref.priorite.(sf)=5) !t
  let nombre_imp n t = List.length (all_imp n t)
  let nth_imp n k t = List.nth (List.sort compare (all_imp n t)) k
  let reord_imp n k t = ()
  let print t = assert false
end


module Cl_g =
struct
  type t = (int*int) list ref
  let empty _ = ref []
  let mem cl n t = List.exists (fun (i,cl') -> i<=n && cl'=cl) !t
  let add i cl t = t := (i,cl):: !t
  let rm i cl t = t := rm (i,cl) !t
end

module Cl_d =
struct
  type t = (int*int) list ref
  let empty _ = ref []
  let mem cl n t = List.mem (n,cl) !t
  let add i cl t = t := (i,cl):: !t
  let rm i cl t = t := rm (i,cl) !t
end

open Def



module type CASE =
sig
  type t
  val empty : t
  val add : couple -> t -> t
  val rm : couple -> t -> t
  val filter_n : int -> t -> (couple list)
  val trouve_n : int -> t -> couple
  val fold_n : int -> ('a -> couple -> 'a) -> 'a -> t -> 'a
end
(*
module Gcases : CASE =
struct
  type t = Lcord.t
  let empty = Lcord.empty
  let add = Lcord.add
  let rm = Lcord.rm
  let filter_n = Lcord.filter_inf
  let trouve_n = Lcord.trouve_inf
  let fold_n = Lcord.fold_inf
end
*)
(*
module Dcases : CASE =
struct
  type t = Lcord.t
  let empty = Lcord.empty
  let add = Lcord.add
  let rm = Lcord.rm
  let filter_n = Lcord.filter_eq
  let trouve_n = Lcord.trouve_eq
  let fold_n _ = assert false
end
*)

module Tab_prio =
  functor (Case:CASE) ->
struct
  type t = Case.t array
  let empty () = Array.make 7 Case.empty
  let add c t = let p = !priorite.(snd c) in t.(p) <- Case.add c t.(p)
  let rm c t = let p = !priorite.(snd c) in t.(p) <- Case.rm c t.(p)
  let all_imp n t = Case.filter_n n t.(5)
  let fold_vars n f init t = Case.fold_n n f init t.(6)
  let priorite_plus_forte n t =
    let rec aux p =
      if p = 7 then 7,(0,0)
      else
	try p,(Case.trouve_n n t.(p))
	with Not_found -> aux (p+1)
    in aux 0

end

module G = Tab_prio(IndXsfs_list)
module D = Tab_prio(Couple_list)




(*-------*)



(*
module Case_cl =
struct
  type t = int list
  let empty = []
  let mem_g n t = List.exists (fun i -> i<=n) t
  let mem_d n t = List.mem n t
  let add i t = i::t
  let rm i t = Utilities.enleve i t
end
*)
module Case_cl = IndXnb_list
module Case_cl_g =
struct
  type t = Case_cl.t
  let empty = Case_cl.empty
  let mem = Case_cl.mem_g
  let add = Case_cl.add
  let rm = Case_cl.rm
end
module Case_cl_d =
struct
  type t = Case_cl.t
  let empty = Case_cl.empty
  let mem = Case_cl.mem_d
  let add = Case_cl.add
  let rm = Case_cl.rm
end




module type CASE_cl =
sig
  type t
  val empty : t
  val mem : int -> t -> bool
  val add : int -> t -> t
  val rm : int -> t -> t
end
module Tab_cl =
  functor (Case : CASE_cl) ->
struct
  type t = Case.t array
  let empty ncl = Array.make ncl Case.empty
  let mem cl n t = Case.mem n t.(cl)
  let add i cl t = t.(cl) <- Case.add i t.(cl) 
  let rm i cl t = t.(cl) <- Case.rm i t.(cl)
end
module Cl_g = Tab_cl(Case_cl_g)
module Cl_d = Tab_cl(Case_cl_d)


(*
module Cl =
struct
  type t = (int list) array
      
  let empty ncl = Array.make ncl []

  let mem_g cl n t = List.exists (fun i -> i<=n) t.(cl)
  let mem_d cl n t = List.mem n t.(cl)
  
  let add i cl t = t.(cl) <- i::t.(cl)
    
  let rm i cl t = t.(cl) <- Utilities.enleve i t.(cl)
end

module Cl_g = 
struct
  type t = Cl.t
  let empty = Cl.empty
  let mem = Cl.mem_g
  let add = Cl.add
  let rm = Cl.rm
end

module Cl_d = 
struct
  type t = Cl.t
  let empty = Cl.empty
  let mem = Cl.mem_d
  let add = Cl.add
  let rm = Cl.rm
end
*)

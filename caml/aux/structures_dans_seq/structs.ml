open Def



module type CASES =
sig
  type t
  val empty : t
  val add : couple -> t -> t
  val rm : couple -> t -> t
  val filter_n : int -> t -> (couple list)
  val trouve_n : int -> t -> couple
  val fold_n : int -> ('a -> couple -> 'a) -> 'a -> t -> 'a
end;;

module Gcases : CASES =
struct
  type t = Lcord.t
  let empty = Lcord.empty
  let add = Lcord.add
  let rm = Lcord.rm
  let filter_n = Lcord.filter_inf
  let trouve_n = Lcord.trouve_inf
  let fold_n = Lcord.fold_inf
end
module Dcases : CASES =
struct
  type t = Lcord.t
  let empty = Lcord.empty
  let add = Lcord.add
  let rm = Lcord.rm
  let filter_n = Lcord.filter_eq
  let trouve_n = Lcord.trouve_eq
  let fold_n _ = assert false
end


module Tab_prio =
  functor (Case:CASES) ->
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

module G = Tab_prio(Gcases)
module D = Tab_prio(Dcases)




(*-------*)



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

open Def
open Global_ref


module type CASE =
sig
  type t
  val empty : t
  val add : couple -> t -> t
  val rm : couple -> t -> t
  (*val filter_n : int -> t -> (couple list)*)
  val trouve_n : int -> t -> couple
  val fold_n : int -> ('a -> couple -> 'a) -> 'a -> t -> 'a

  val nombre_n : int -> t -> int
  val nth_n : int -> int -> t -> (couple * t)
  val reord_n : int -> int -> t -> t

  val print : t -> unit
end

module Case = IndXsfs_list

module Case_g : CASE =
struct
  type t = Case.t
  let empty = Case.empty
  let add = Case.add
  let rm = Case.rm
  (*let filter_n = Case.filter_inf*)
  let trouve_n = Case.trouve_inf
  let fold_n = Case.fold_inf

  let nombre_n = Case.nombre_inf
  let nth_n = Case.nth_inf
  let reord_n = Case.reord_inf

  let print = Case.print

  (*let add x = Format.printf "G  "; add x
  let rm x = Format.printf "G  "; rm x*)
end

module Case_d : CASE =
struct
  type t = Case.t
  let empty = Case.empty
  let add = Case.add
  let rm = Case.rm
  (*let filter_n = Case.filter_eq*)
  let trouve_n = Case.trouve_eq
  let fold_n _ = assert false

  let nombre_n = Case.nombre_eq
  let nth_n = Case.nth_eq
  let reord_n = Case.reord_eq

  let print = Case.print

  (*let add x = Format.printf "D  "; add x
  let rm x = Format.printf "D  "; rm x*)
end



module Tab_prio =
  functor (Case:CASE) ->
struct
  type t = Case.t array
  let empty () = Array.make 7 Case.empty
  let add c t = let p = !priorite.(snd c) in t.(p) <- Case.add c t.(p)
  let rm c t = let p = !priorite.(snd c) in t.(p) <- Case.rm c t.(p)
  (*let all_imp n t = Case.filter_n n t.(5)*)
  let fold_vars n f init t = Case.fold_n n f init t.(6)
  let priorite_plus_forte n t =
    let rec aux p =
      if p = 6 then 6,(0,0)
      else
	try p,(Case.trouve_n n t.(p))
	with Not_found -> aux (p+1)
    in aux 1

  let nombre_imp n t = Case.nombre_n n t.(5)
  let nth_imp n k t = 
    let (c,case) = Case.nth_n n k t.(5) in
    t.(5) <- case;
    c
  let reord_imp n k t = t.(5) <- Case.reord_n n k t.(5)

(*
  let add c t = let p = !priorite.(snd c) in
		Format.printf "add prio=%d  " p;
		t.(p) <- Case.add c t.(p)
  let rm c t = let p = !priorite.(snd c) in 
	       Format.printf "rm prio=%d  " p;
	       t.(p) <- Case.rm c t.(p)
*)

  let print t = for p=0 to 6 do Case.print t.(p) done
end

module G = Tab_prio(Case_g)
module D = Tab_prio(Case_d)




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

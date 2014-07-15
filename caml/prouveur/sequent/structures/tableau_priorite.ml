open Global_ref

type sous_formule = int
type couple = int*sous_formule



module type CASE =
sig
  type t
  val empty : t
  val add : couple -> t -> t
  val rm : couple -> t -> t
  val trouve_n : int -> t -> couple
  val fold_n : int -> ('a -> couple -> 'a) -> 'a -> t -> 'a

  val nombre_n : int -> t -> int
  val nth_n : int -> int -> t -> (couple * t)
  val reord_n : int -> int -> t -> t

  val print : t -> unit
end

module Main =
  functor (Case:CASE) ->
struct
  type t = Case.t array
  let empty () = Array.make 7 Case.empty
  let add c t = let p = !priorite.(snd c) in t.(p) <- Case.add c t.(p)
  let rm c t = let p = !priorite.(snd c) in t.(p) <- Case.rm c t.(p)
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

  let print t = for p=0 to 6 do Case.print t.(p) done
end

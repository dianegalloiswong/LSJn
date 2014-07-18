open Global_ref

type sous_formule = int
type couple = int*sous_formule



module type CASE =
sig
  type t
  val empty : t
  val mem : int -> t -> bool
  val add : int -> t -> t
  val rm : int -> t -> t
end

module Main =
  functor (Case : CASE) ->
struct
  type t = Case.t array
  let empty ncl = Array.make ncl Case.empty
  let mem cl n t = Case.mem n t.(cl)
  let add i cl t = t.(cl) <- Case.add i t.(cl) 
  let rm i cl t = t.(cl) <- Case.rm i t.(cl)
end


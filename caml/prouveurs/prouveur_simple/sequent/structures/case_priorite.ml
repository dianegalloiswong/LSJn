module Case = IndiceXsflist_list

module G =
struct
  type t = Case.t
  let empty = Case.empty
  let add = Case.add
  let rm = Case.rm
  let trouve_n = Case.trouve_inf
  let fold_n = Case.fold_inf

  let nombre_n = Case.nombre_inf
  let nth_n = Case.nth_inf
  let reord_n = Case.reord_inf

  let print = Case.print
end

module D =
struct
  type t = Case.t
  let empty = Case.empty
  let add = Case.add
  let rm = Case.rm
  let trouve_n = Case.trouve_eq
  let fold_n _ = assert false

  let nombre_n = Case.nombre_eq
  let nth_n = Case.nth_eq
  let reord_n = Case.reord_eq

  let print = Case.print
end



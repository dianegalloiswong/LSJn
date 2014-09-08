module Case = IndiceXoccurrences_list

module G =
struct
  type t = Case.t
  let empty = Case.empty
  let mem = Case.mem_g
  let add = Case.add
  let rm = Case.rm
end

module D =
struct
  type t = Case.t
  let empty = Case.empty
  let mem = Case.mem_d
  let add = Case.add
  let rm = Case.rm
end


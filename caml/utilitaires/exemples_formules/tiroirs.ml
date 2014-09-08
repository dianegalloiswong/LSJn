open Def

let var i j = FVar ("p_"^(string_of_int i)^"_"^(string_of_int j))

let g p h =
  let rec aux1 i j acc =
    if j = h then acc
    else aux1 i (j+1) (F (Ou,acc,var i j))
  in
  let ou_h i = aux1 i 1 (var i 0) in
  let rec aux2 i acc =
    if i = p then acc
    else aux2 (i+1) (F (Et,acc,ou_h i))
  in
  aux2 1 (ou_h 0)

let d p h =
  let et i1 i2 j = F (Et,var i1 j,var i2 j) in
  let rec aux i1 i2 j acc =
    if i1 = p-1 then acc
    else if i2 = p then aux (i1+1) (i1+2) 0 acc
    else if j = h then aux i1 (i2+1) 0 acc
    else aux i1 i2 (j+1) (F (Ou,acc,et i1 i2 j))
  in
  aux 0 1 1 (et 0 1 0)


let main p h = F (Imp,g p h,d p h)

let attendu p h = let b = p > h in (b,b)






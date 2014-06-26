open Def











let tiroirs p h =
  let var i j = FVar ("p_"^(string_of_int i)^"_"^(string_of_int j)) in
  let g =
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
  in
  let d =
    let et i1 i2 j = F (Et,var i1 j,var i2 j) in
    let rec aux i1 i2 j acc =
      if i1 = p-1 then acc
      else if i2 = p then aux (i1+1) (i1+2) 0 acc
      else if j = h then aux i1 (i2+1) 0 acc
      else aux i1 i2 (j+1) (F (Ou,acc,et i1 i2 j))
    in
    aux 0 1 1 (et 0 1 0)
  in
  F (Imp,g,d)



let eq_boucle n =
  let var n = FVar ("p"^(string_of_int n)) in
  let rec eq_chemin n =
    if n = 2 then
      F(Et, F(Imp,var 1,var 2), F(Imp,var 2,var 1))
    else
      let f1 = eq_chemin (n-1) in
      let f2 = F(Et, F(Imp,var (n-1),var n), F(Imp,var n,var (n-1))) in
      F(Ou,f1,f2)
  in
  let f1 = eq_chemin n in
  let f2 = F(Et, F(Imp,var n,var 1), F(Imp,var 1,var n)) in
  F(Ou,f1,f2)





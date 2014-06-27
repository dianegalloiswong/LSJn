open Def

let var n = FVar ("p"^(string_of_int n))

let rec eq_chemin n =
  if n = 2 then
    F(Et, F(Imp,var 1,var 2), F(Imp,var 2,var 1))
  else
    let f1 = eq_chemin (n-1) in
    let f2 = F(Et, F(Imp,var (n-1),var n), F(Imp,var n,var (n-1))) in
    F(Ou,f1,f2)

let main n =
  let f1 = eq_chemin n in
  let f2 = F(Et, F(Imp,var n,var 1), F(Imp,var 1,var n)) in
  F(Ou,f1,f2)

open Def
open Global_ref

let connecteur = function Et -> " & " | Ou -> " v " | Imp -> " -> "
let rec formule = function
  | FFaux -> "faux"
  | FVar v -> v
  | F (Imp,f,FFaux) -> let s = formule f in "non ("^s^")"
  | F (c,a,b) ->
    let sa = formule a and sb = formule b in
    let sc = connecteur c in
    "("^sa^sc^sb^")"


let case_sf = function
  | CFaux -> "Faux"
  | CVar x -> "Var "^x
  | C (conn,i1,i2) -> 
    let s1 = string_of_int i1
    and sc = connecteur conn
    and s2 = string_of_int i2 in
    s1^sc^s2


let rec to_real_formule a = match !sf.(a) with
  | CFaux -> FFaux
  | CVar s -> FVar s
  | C (conn,a1,a2) -> F (conn,to_real_formule a1,to_real_formule a2)

let sous_formule a = formule (to_real_formule a)



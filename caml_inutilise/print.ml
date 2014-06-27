open Def

let () = assert false

let connecteur_to_string = function Et -> " & " | Ou -> " v " | Imp -> " -> "
let rec formule_to_string = function
  | FFaux -> "faux"
  | FVar v -> v
  | F (Imp,f,FFaux) -> let s = formule_to_string f in "non ("^s^")"
  | F (c,a,b) ->
    let sa = formule_to_string a and sb = formule_to_string b in
    let sc = connecteur_to_string c in
    "("^sa^sc^sb^")"
let formule f = Format.printf "%s" (formule_to_string f)

let print_tab_int t =
  for i=0 to (Array.length t)-1 do
    Format.printf "%d " t.(i)
  done

let case_to_string = function
  | CFaux -> "Faux"
  | CVar x -> "Var "^x
  | C (conn,i1,i2) -> 
    let s1 = string_of_int i1
    and sc = connecteur_to_string conn
    and s2 = string_of_int i2 in
    s1^sc^s2

let sous_formules sf classe =
  for i=0 to (Array.length sf)-1 do
    Format.printf "  %d %d %s@." i classe.(i) (To_string.case_sf sf.(i))
  done

let rec to_real_formule i = match !sf.(i) with
  | CFaux -> FFaux
  | CVar s -> FVar s
  | C (conn,i1,i2) -> F (conn,to_real_formule i1,to_real_formule i2)

let sous_formule_to_string i = formule_to_string (to_real_formule i)
let sous_formule a = Format.printf "%s" (sous_formule_to_string a)



let couple_list l =
  Format.printf "[";
  List.iter (fun (i,a) -> Format.printf " (%d,%d);" i a) l;
Format.printf " ]"




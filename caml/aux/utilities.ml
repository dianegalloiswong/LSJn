open Def




let rec enleve x = function
  | [] -> raise Not_found
  | h::t when h=x -> t
  | h::t -> h::(enleve x t)

(*let rec select descr = function
  | [] -> raise Not_found
  | h::t when descr h -> h,t
  | h::t -> let (x,l) = select descr t in (x,h::l)*)

(*let rec trouve descr = function
  | [] -> raise Not_found
  | h::_ when descr h -> h
  | _::t -> trouve descr t
= List.find*)


let rec insert_ord i = function
  | [] -> [i]
  | h::t when h<i -> h::(insert_ord i t)
  | l -> i::l





(* impression *)

let connecteur_to_string = function Et -> " & " | Ou -> " v " | Imp -> " -> "
let rec formule_to_string = function
  | FFaux -> "faux"
  | FVar v -> v
  | F (Imp,f,FFaux) -> let s = formule_to_string f in "non ("^s^")"
  | F (c,a,b) ->
    let sa = formule_to_string a and sb = formule_to_string b in
    let sc = connecteur_to_string c in
    "("^sa^sc^sb^")"


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

let print_sous_formules sf classe =
  for i=0 to (Array.length sf)-1 do
    Format.printf "  %d %d %s@." i classe.(i) (case_to_string sf.(i))
  done

let rec to_real_formule i = match !sf.(i) with
  | CFaux -> FFaux
  | CVar s -> FVar s
  | C (conn,i1,i2) -> F (conn,to_real_formule i1,to_real_formule i2)

let sous_formule_to_string i = formule_to_string (to_real_formule i)




let print_couple_list l =
  Format.printf "[";
  List.iter (fun (i,a) -> Format.printf " (%d,%d);" i a) l;
Format.printf " ]"




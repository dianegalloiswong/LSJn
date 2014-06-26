open Def

type regle = R_fauxL | R_Id | R_etL | R_etR | R_ouL | R_ouR | R_impL | R_impR
(*| R_Irr | R_etR1 | R_etR2 | R_ouL1 | R_ouL2 | R_impL1 | R_impL2 | R_Succ*)
type preuve = P of regle*sous_formule*(preuve list)

type modele_arbre = M of (string list)*(modele_arbre list)

type reponse = Preuve of preuve | CMod of modele_arbre


let vrai = function Preuve _ -> true | CMod _ -> false

let preuve = function Preuve pr -> pr | CMod _ -> assert false
let preuves = List.map preuve
let cmod = function Preuve _ -> assert false | CMod m -> m
let cmods = List.map cmod


exception Continuer of reponse
(* quand on trouve une réfutation d'un prémice non inversible (impL et impR) *)


let regle_to_string = function | R_fauxL -> "fauxL" | R_Id -> "Id" | R_etL -> "etL" | R_etR -> "etR" | R_ouL -> "ouL" | R_ouR -> "ouR" | R_impL -> "impL" | R_impR -> "impR"



let print_preuve preuve =
  let print_noeud regle a chemin =
    Format.printf "  (Preuve)  [ ";
    List.iter (fun n -> Format.printf "%d " n) (List.rev chemin);
    Format.printf "] : %s , %s@." (regle_to_string regle) (Utilities.sous_formule_to_string a)
  in
  let rec aux chemin = function P (regle,a,fils) ->
    print_noeud regle a chemin;
    aux_liste chemin 0 fils
  and aux_liste chemin i = function
    | [] -> ()
    | h::t -> aux (i::chemin) h; aux_liste chemin (i+1) t
  in aux [] preuve

let print_modele_arbre a =
  let print_noeud vars chemin =
    Format.printf "  (C-M)  [ ";
    List.iter (fun n -> Format.printf "%d " n) (List.rev chemin);
    Format.printf "] : ";
    List.iter (fun s -> Format.printf "%s " s) vars;
    Format.printf "@."
  in
  let rec aux chemin = function M (vars,fils) -> 
    print_noeud vars chemin;
    aux_liste chemin 0 fils
  and aux_liste chemin i = function
    | [] -> ()
    | h::t -> aux (i::chemin) h; aux_liste chemin (i+1) t
  in aux [] a


let print_rep_details rep =
  if vrai rep then
    (Format.printf "vrai@.";
     print_preuve (preuve rep))
  else
    (Format.printf "faux@.";
     print_modele_arbre (cmod rep))


let print_rep rep =
  if vrai rep then
    Format.printf "vrai@."
  else
    Format.printf "faux@."

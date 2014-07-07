type regle = R_fauxL | R_Id | R_etL | R_etR | R_ouL | R_ouR | R_impL | R_impR
type t = P of regle*int*(t list) (* int : sous-formule *)

let regle_to_string = function | R_fauxL -> "fauxL" | R_Id -> "Id" | R_etL -> "etL" | R_etR -> "etR" | R_ouL -> "ouL" | R_ouR -> "ouR" | R_impL -> "impL" | R_impR -> "impR"


let print preuve =
  let print_noeud regle a chemin =
    Format.printf "  (Preuve)  [ ";
    List.iter (fun n -> Format.printf "%d " n) (List.rev chemin);
    Format.printf "] : %s , %s@." (regle_to_string regle) (To_string.sous_formule a)
  in
  let rec aux chemin = function P (regle,a,fils) ->
    print_noeud regle a chemin;
    aux_liste chemin 0 fils
  and aux_liste chemin i = function
    | [] -> ()
    | h::t -> aux (i::chemin) h; aux_liste chemin (i+1) t
  in aux [] preuve












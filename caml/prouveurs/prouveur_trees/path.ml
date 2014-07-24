let path_prouveur_trees = "prouveurs/prouveur_trees/"
let path_tmp_trees = path_prouveur_trees^"code_trees_genere/"

let ext_trees = ".trees"

(*let fonctions_sf = path_tmp_trees^"fonctions_sf"^ext_trees*)
let code_trees = path_tmp_trees^"code"^ext_trees

let liste_code_trees_fixe_aux = [
  "utilities";
  "sequent";
  "plus_forte_priorite";
  "all_imp";
  "prouvable";
  "expr";
]
let liste_code_trees_fixe = List.map (fun s -> path_prouveur_trees^"code_trees_fixe/"^s^ext_trees) liste_code_trees_fixe_aux



(* compilateur vers caml *)
let code_caml = path_tmp_trees^"code_caml.ml"
let code_caml_executable = path_tmp_trees^"code_caml"
let code_caml_sortie = path_tmp_trees^"code_caml_sortie.txt"
let code_caml_errors = path_tmp_trees^"code_caml_errors.txt"
let entete_caml = path_prouveur_trees^"code_trees_fixe/entete_caml.ml"


(* vers coq *)
let code_coq = path_tmp_trees^"code_coq.txt"

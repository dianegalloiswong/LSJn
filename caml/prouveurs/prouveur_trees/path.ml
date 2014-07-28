let path_prouveur_trees = "prouveurs/prouveur_trees/"
let path_tmp_trees = "tmp/"
(*path_prouveur_trees^"code_trees_genere/"*)

let ext_trees = ".trees"


let nom_defaut = "code"
let nom = ref nom_defaut
let fichier_seul s =
  let len = String.length s in
  let rec aux n =
    if n = -1 then s 
    else if s.[n] = '/' then String.sub s (n+1) (len-n-1) 
    else aux (n-1)
  in aux (len-1)
let set_nom s = nom := fichier_seul s
let formule = ref (None : Def.formule option)
let reset () = nom:=nom_defaut; formule:=None



let code_trees () = path_tmp_trees^(!nom)^ext_trees

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
let entete_caml = path_prouveur_trees^"code_trees_fixe/entete_caml.ml"
let code_caml () = path_tmp_trees^(!nom)^"_caml.ml"
let code_caml_executable () = path_tmp_trees^(!nom)^"_caml"
let code_caml_sortie () = path_tmp_trees^(!nom)^"_caml_sortie.txt"
(*let code_caml_errors () = path_tmp_trees^(!nom)^"_caml_errors.txt"*)



(* vers coq *)
let code_coq () = path_tmp_trees^(!nom)^".v"
(*
let bin () = path_tmp_trees^(!nom)^"_binaire.txt"
let bin_sortie () = path_tmp_trees^(!nom)^"_binaire_sortie.txt"
*)
let bin = path_tmp_trees^"binaire.txt"

(* directement en caml *)
let entete_caml_direct = path_prouveur_trees^"code_trees_fixe/entete_caml_direct.ml"
let code_caml_direct () = path_tmp_trees^(!nom)^"_caml_direct.ml"

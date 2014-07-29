let path_prouveur_trees = "prouveurs/prouveur_trees/"
let path_tmp = "tmp/"
(*path_prouveur_trees^"code_trees_genere/"*)

let ext_trees = ".trees"


let nom_defaut = "code"
let nom = ref nom_defaut
let fichier_seul s =
  let len = String.length s in
  for i=0 to len-1 do
    if s.[i]='+' then s.[i]<-'x';
    if s.[i]='.' then s.[i]<-'_';
    if s.[i]='/' then s.[i]<-'_';
  done;
  s
  (*let rec cherche_slash_point n pos_point =
    if n = -1 then -1,pos_point
    else if s.[n]='.' && pos_point=len then cherche_slash_point (n-1) n
    else if s.[n]='/' then n,pos_point
    else (if s.[n]='+' then s.[n]<-'x';
	  if s.[n]='.' then s.[n]<-'_';
	  cherche_slash_point (n-1) pos_point)
  in
  let pos_slash,pos_point = cherche_slash_point (len-1) len in
  String.sub s (pos_slash+1) (pos_point-1-pos_slash)*)
  (*let rec enleve_slash_point n =
    if n = -1 then s 
    else if s.[n] = '.' then enleve_slash_point
    else if s.[n] = '/' then String.sub s (n+1) (len-n-1) 
    else enleve_slash_point (n-1)
  in enleve_slash_point (len-1)*)
let set_nom s = nom := fichier_seul s
let formule = ref (None : Def.formule option)
let reset () = nom:=nom_defaut; formule:=None



let code_trees () = path_tmp^(!nom)^ext_trees

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

let path_caml = path_tmp^"caml/"
let code_caml() = path_caml^(!nom)^".ml"
let code_caml_executable() = path_caml^(!nom)^".out"
let code_caml_sortie() = path_caml^(!nom)^"_sortie.txt"
(*let code_caml () = path_tmp^(!nom)^"_caml.ml"
let code_caml_executable () = path_tmp^(!nom)^"_caml"
let code_caml_sortie () = path_tmp^(!nom)^"_caml_sortie.txt"*)
(*let code_caml_errors () = path_tmp^(!nom)^"_caml_errors.txt"*)



(* vers coq *)
let path_coq = path_tmp^"coq/"
let code_coq () = path_coq^(!nom)^".v"
(*
let bin () = path_tmp^(!nom)^"_binaire.txt"
let bin_sortie () = path_tmp^(!nom)^"_binaire_sortie.txt"
*)
let bin s = path_coq^s^".out"

(* directement en caml *)
let entete_caml_direct = path_prouveur_trees^"code_trees_fixe/entete_caml_direct.ml"
let code_caml_direct () = (*path_tmp^(!nom)^"_caml_direct.ml"*) code_caml()

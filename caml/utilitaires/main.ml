
let formule_details = ref None
let ma_liste = ref false
let ma_liste_courte = ref false
let arg1 = ref 0

let tiroirs_spec = Arg.Tuple [ Arg.Set_int arg1;
  Arg.Int (fun arg2 -> formule_details := Some (
    Tiroirs.main !arg1 arg2,
    Some (!arg1 > arg2),
    "ph_"^(string_of_int !arg1)^"_"^(string_of_int arg2)
  )) ]
let eq_boucle n = formule_details := Some (
  Eq_boucle.main n,
  Some false,
  "eqb_"^(string_of_int n)
)



let options = [
(* choix du prouveur *)
  (* par défaut : "prouveur simple"*)
  "-T", Arg.Set Options.trees, ": utilise le \"prouveur T\"";
  "-TM", Arg.Set Options.trees_machine, ": utilise le \"prouveur T M\"";
  "-compile-caml", Arg.Set Options.compile_caml, ": utilise le \"prouveur compilé caml\"";
  "-all", Arg.Set Options.all, ": utilise successivement \"prouveur simple\", \"prouveur T\", \"prouveur T M\", \"prouveur compilé caml\" (et génère un fichier results.txt qui contient les différents temps mis sous forme de tableau, mais il y a parfois des problèmes avec la mise en page du tableau)";

  "-indexation", Arg.Set Options.indexation, ": affiche un tableau correspondant à l'étape d'indexation ; ne lance aucune recherche de preuve";

(* affichage *)
  "-f", Arg.Set Options.affiche_formule_ref, ": affiche la formule";
  "-resultat", Arg.Set Options.affiche_rep_ref, ": affiche le résultat (\"vrai\" ou \"faux\")";
  "-rien-afficher", Arg.Set Options.rien_afficher, ": n'affiche que le temps total, sauf si on trouve un résultat différent de celui attendu";
  "-notime", Arg.Set Options.notime, ": ne chronomètre pas";
  "-stop", Arg.Set_float Options.temps_max, "<f:float> : arrête si non terminé après f secondes";

(* formule(s) à traiter *)
  (* nom de fichier en .p ou nom de répertoire en contenant *)
  "-ph", tiroirs_spec, "<p:int> <h:int> : traite la formule suivante : principe du pigeonnier avec p pigeons et h trous";
  "-eqb", Arg.Int eq_boucle, "<n:int> : traite la formule suivante : (p1 <-> p2) | (p2 <-> p3) | ... | (pn <-> p1)";
  "-liste", Arg.Set ma_liste, ": traite une liste de quelques formules courtes";
  "-liste-courte", Arg.Set ma_liste_courte, ": traite une liste de quelques formules très courtes";



(*
  "-trees", Arg.Set Options.trees, ": utilise le prouveur \"trees\" au lieu du prouveur caml";
  "-trees-machine", Arg.Set Options.trees_machine, ": passe aussi par le .trees mais compile un code exécuté par une machine abstraite";
  "-compile-caml", Arg.Set Options.compile_caml, ": génère un code caml dépendant de la formule et le compile avec ocamlc";
  "-trees-via-caml", Arg.Set Options.trees_via_caml, ": génère le code \"trees\", le compile en caml et l'exécute avec ocamlc";
  "-all", Arg.Set Options.all, ": les quatre prouveurs";

  (* général *)
  "-indexation", Arg.Set Options.indexation, ": affiche formules et contenu des tableaux sf et classe ; ne lance pas la recherche de preuve";
  "-f", Arg.Set Options.affiche_formule_ref, ": affiche la formule";
  "-rep", Arg.Set Options.affiche_rep_ref, ": affiche le résultat (\"vrai\" ou \"faux\")";
  "-rien-afficher", Arg.Set Options.rien_afficher, ": n'affiche que le temps total, sauf si on trouve un résultat faux";
  "-notime", Arg.Set Options.notime, ": ne chronomètre pas";
  "-stop", Arg.Set_float Options.temps_max, "<f:float> : arrête si pas fini après f secondes";
  (*
  "-details", Arg.Unit details, "affiche les preuves et contre-modèles";
  "-preuves", Arg.Set Options.preuves, "affiche les preuves";
  "-cmods", Arg.Set Options.cmods, "affiche les contre-modèles";
  *)

  (* nom de fichier en .p ou nom de répertoire en contenant *)

  (* autre(s) formule(s) à traiter *)
  "-ph", tiroirs_spec, "<p:int> <h:int> : exécute sur le principe du pigeonnier avec p pigeons et h trous";
  "-eqb", Arg.Int eq_boucle, "<n:int> : exécute sur une boucle de n équivalences";
  "-liste", Arg.Set ma_liste, ": exécute sur une liste de quelques formules courtes";
  "-liste-courte", Arg.Set ma_liste_courte, ": exécute sur une liste de quelques formules très courtes";

  (* debug *)
  "-irr", Arg.Set Options.irr, ": affiche les utilisations de l'axiome de réfutation Irr";

  "-comp", Arg.Set Options.compile_test, ": avec la simulation de recherche compilée";
*)
]

(*
let usage = "Par défaut : affiche nom de fichier éventuel, temps mis pour décider si la formule est prouvable, et résultat obtenu seulement s'il est différent de celui attendu.\n  <nom de fichier en .p> (syntaxe des problèmes d'ILTP) : exécute sur la formule décrite par le fichier, en comparant avec le résultat attendu donné dans les commentaires\n  <nom de répertoire> : cherche récursivement les fichiers en .p"
*)
(* On dispose de plusieurs implémentations de prouveurs de logique intuitionniste propositionnelle. *)

let usage = "Le prouveur utilisé par défaut est le \"prouveur simple\", qui est le plus efficace.
Formule(s) à traiter :
   <nom de fichier en .p> (syntaxe des problèmes d'ILTP) : exécute sur la formule décrite par le fichier, en comparant avec le résultat attendu donné dans les commentaires
   <nom de répertoire> : cherche récursivement les fichiers en .p
   voir aussi les flags -liste, -liste-courte, -ph, -eqb
Affichage par défaut : nom de la formule traitée (lié au nom du fichier en .p s'il y en a un), temps mis pour décider si elle est prouvable et nombre d'appels à la fonction récursive principale. On n'affiche en général pas le résultat (prouvable ou non), mais on compare ce résultat à celui qu'on devait trouver, et si ce n'est pas le même, on le signale."


let noms = ref []





(* parsing de la ligne de commande *)

let () = Arg.parse options (fun nom -> noms := nom:: !noms) usage


let () = if !Options.all then Test_all.init ()




(* fichiers et répertoires *)

let do_file nom =
  if Options.affiche_nom_fichier() then Format.printf "%s@." nom;
  try
    let att,f = Analyser_ILTP.main nom in
    Exec_formule.main (f,att,nom)
  with Exit -> Format.printf "Exit.@."

let pointp nom = let n = String.length nom in nom.[n-2]='.' && nom.[n-1]='p'

let () = List.iter (fun nom ->
  if pointp nom && Time.faire_fichier nom then do_file nom
) (List_files.main !noms)




(* formule seule *)

let () = match !formule_details with Some f -> Exec_formule.main f | None -> ()




(* listes prédéfinies de formules *)

let () =
  if !ma_liste then
    List.iter Exec_formule.main Quelques_formules.l

let () =
  if !ma_liste_courte then
    List.iter Exec_formule.main Quelques_formules.l1

  



(* affichage final *)

let () = 
  if Options.affiche_temps_total() then Format.printf "temps total : %fs@." !Time.temps_total;
  if !Time.echoues > 0 then Format.printf "non terminés en moins de %fs : %d@." !Options.temps_max !Time.echoues;
  if !Time.non_traites > 0 then Format.printf "non traités (car problème plus facile non terminé) : %d@." !Time.non_traites

let () = if !Options.all then Test_all.close ()



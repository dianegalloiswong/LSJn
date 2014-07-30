open Tree

let tree_to_bool = function
  | Int 0 -> false
  | Int _ -> true
  | t -> Format.eprintf "Le résultat de l'exécution n'est pas un booléen@."; Tree.print t; raise Exit

let exec_direct prog =
  if Options.affiche_temps_etapes() then Format.printf "exécution du .trees : %!";
  Time.time Interp_trees.main prog

let exec_via_machine prog =
  let p = Convert_ast_pos.main (fst prog) in
  if Options.affiche_temps_etapes() then Format.printf "compilation du binaire : %!";
  let bin = Time.time Compile_trees_vers_machine.main p in
  if Options.affiche_temps_etapes() then Format.printf "exécution du binaire : %!";
  Time.time Machine_abstraite.main bin


let main (formule : Def.formule) : bool =
  if Options.affiche_temps_etapes() then Format.printf "compilation vers trees : %!";
  Time.time Compile_vers_trees.main formule;
  let prog = Analyser_trees.main (Path.code_trees()) in

  let exec = if !Options.trees_machine then exec_via_machine else exec_direct in
  let t = exec prog in

  tree_to_bool t




 (* Compile_trees_vers_coq.main prog;*)
  (*Exec_bin.main ();*)
  


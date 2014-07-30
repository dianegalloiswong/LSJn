open Tree

let tree_to_bool = function
  | Int 0 -> false
  | Int _ -> true
  | t -> Format.eprintf "Le résultat de l'exécution n'est pas un booléen@."; Print_ast_trees.tree t; raise Exit


let exec_machine prog =
  let p = Convert_ast_pos.main (fst prog) in
  Format.printf "compilations du binaire : %!";
  let bin = Time.time Compiler_coq.main p in
  Format.printf "exécution du binaire : %!";
  Time.time Machine_abstraite.main bin

let exec_trees prog =
  Format.printf "exécution du .trees : %!";
  Time.time Exec_trees.main prog

let main (f : Def.formule) : bool =
  if Options.affiche_temps_etapes() then Format.printf "compilation vers trees : %!";
  Time.time Compile_vers_trees.main f;
  let prog = Analyser_trees.main (Path.code_trees()) in

  let t = exec_trees prog in
  (*let t = exec_machine prog in*)

  tree_to_bool t




 (* Compile_trees_vers_coq.main prog;*)
  (*Exec_bin.main ();*)
  


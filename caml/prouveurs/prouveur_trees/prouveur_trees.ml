open Tree

let exec_maison prog = 
  let t = Exec_trees.main prog in
  match t with
    | Int 0 -> false
    | Int _ -> true
    | _ -> Format.eprintf "Le résultat de l'exécution n'est pas un booléen@."; Print_ast_trees.tree t; raise Exit


let main f =
  Format.printf "compilation vers trees : %!";
  Time.time Compile.main f;
  let prog = Analyser_trees.main Path.code_trees in

  Compile_trees_vers_coq.main prog;

  if !Options.trees_via_caml then
    Exec_trees_via_ocamlc.main prog
  else
    exec_maison prog

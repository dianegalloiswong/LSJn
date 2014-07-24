open Tree

let exec_maison prog = 
  let t = Exec_trees.main prog in
  match t with
    | Int 0 -> false
    | Int _ -> true
    | _ -> Format.eprintf "Le résultat de l'exécution n'est pas un booléen@."; Print.tree t; raise Exit


let main f =
  Format.printf "compilation vers trees : %!";
  Time.time Compile.main f;
  let prog = Analyser_trees.main Path.code_trees in
  if false then
    exec_maison prog
  else
    Exec_trees_via_ocamlc.main prog

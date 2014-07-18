open Tree

let main f =
  Compile.main f;
  let t = Exec_trees.main Compile.code in
  match t with
    | Int 0 -> false
    | Int _ -> true
    | _ -> Format.eprintf "Le résultat de l'exécution n'est pas un booléen@."; Print.tree t; raise Exit

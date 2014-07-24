open Path

(*
let liste_cat = fonctions_sf ::
  (List.map (fun s -> path_trees^"code_trees_fixe/"^s^ext) liste_code_trees_fixe)

let string_cat =
  (List.fold_left (fun s s1 -> s^" "^s1) "cat" liste_cat)
    ^" > "^code
*)


let string_cat =
  (List.fold_left (fun s s1 -> s^" "^s1) "cat" liste_code_trees_fixe)
    ^" >> "^code_trees




let main f =
  let fd = Unix.openfile (*fonctions_sf*)code_trees [Unix.O_WRONLY;Unix.O_CREAT;Unix.O_TRUNC] 0o640 in
  let out = Unix.out_channel_of_descr fd in
  Format.set_formatter_out_channel out;

  Format.printf "\n(* fonctions dépendant de la formule *)\n";
  Format.printf "\n(*\n%s@." (To_string.formule f);


  Init_sf_classe.test f;
  Init_priorite.main ();
  Cote.remplir ();
  Make_fonctions_sf.remplir_fonctions ();

  Format.printf "*)\n@.";

  List.iter (fun df -> Print.decl_func df;Format.printf"@.") (List.rev !Make_fonctions_sf.fonctions);

  List.iter (fun df -> Print.decl_func df;Format.printf"@.") (List.rev (Make_call_num.fonctions_call ()));

  Format.printf "\n(****************************************)\n\n(* code fixe *)\n@.";

  Format.set_formatter_out_channel stdout;
  Unix.close fd;

  ignore (Unix.system string_cat)

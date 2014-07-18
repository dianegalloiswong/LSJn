let main f =
  let fd = Unix.openfile "code_trees_genere/fonctions_sf.ml" [Unix.O_WRONLY;Unix.O_CREAT] 0o640 in
  let out = Unix.out_channel_of_descr fd in
  Format.set_formatter_out_channel out;

  Format.printf "(*\nfichier fonctions_sf\n\n%s@." (To_string.formule f);

  Init_sf_classe.test f;
  Init_priorite.main ();
  Precalculs.remplir_cote ();
  Precalculs.remplir_fonctions ();

  Format.printf "\n*)@.";

  List.iter (fun df -> Print.decl_func df;Format.printf"@.") (List.rev !Make_fonctions_sf.fonctions);

  List.iter (fun df -> Print.decl_func df;Format.printf"@.") (List.rev (Make_call_num.fonctions_call ()));

  Format.set_formatter_out_channel stdout;
  Unix.close fd;

  ignore (Unix.system "cat code_trees_fixe/utilities.ml code_trees_fixe/sequent.ml code_trees_genere/fonctions_sf.ml code_trees_fixe/plus_forte_priorite.ml code_trees_fixe/all_imp.ml code_trees_fixe/prouvable.ml code_trees_fixe/expr.ml > code_trees_genere/code.ml")

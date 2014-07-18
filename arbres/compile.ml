let main f =
  let fd = Unix.openfile "code_genere/fonctions_sf.ml" [Unix.O_WRONLY;Unix.O_CREAT] 0o640 in
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

  ignore (Unix.system "cat code_fixe/utilities.ml code_fixe/sequent.ml code_genere/fonctions_sf.ml code_fixe/plus_forte_priorite.ml code_fixe/all_imp.ml code_fixe/prouvable.ml code_fixe/expr.ml > code_genere/code.ml")

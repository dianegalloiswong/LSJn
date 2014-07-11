open Arbres_ast

open Def



let f = Quelques_formules.f4




let () = Format.printf "%s@." (To_string.formule f)

let () =
  Init_sf_classe.test f;
  Init_priorite.main ();
  Precalculs.remplir_cote ();
  Precalculs.remplir_fonctions ()


let () = List.iter (fun df -> Arbres_print.decl_func df;Format.printf"@.") (List.rev !Fonctions_de_sf.fonctions)







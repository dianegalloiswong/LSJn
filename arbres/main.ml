open Ast

open Def


(*
let f = Quelques_formules.f4




let () = Format.printf "%s@." (To_string.formule f)

let () =
  Init_sf_classe.test f;
  Init_priorite.main ();
  Precalculs.remplir_cote ();
  Precalculs.remplir_fonctions ()


let () = List.iter (fun df -> Print.decl_func df;Format.printf"@.") (List.rev !Fonctions_de_sf.fonctions)
*)


(*
let () = Print.decl_func (Appel_fonction_numero.test 13)
*)


let () = Exec.main "aux_texte.ml"

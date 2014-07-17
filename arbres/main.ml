open Ast



let fonctions_de_f () =
  let f = Quelques_formules.f4 in
  let f = Def.FFaux in
  let f = Quelques_formules.f1 in
  let () = Format.printf "(*\nfichier fonctions_sf\n\n%s@." (To_string.formule f) in

  let () =
    Init_sf_classe.test f;
    Init_priorite.main ();
    Precalculs.remplir_cote ();
    Precalculs.remplir_fonctions ()
  in
  Format.printf "\n*)@.";

  List.iter (fun df -> Print.decl_func df;Format.printf"@.") (List.rev !Make_fonctions_sf.fonctions);

  List.iter (fun df -> Print.decl_func df;Format.printf"@.") (Make_call_num.fonctions_call ());

  ()


let appel_numero () = Print.decl_func (Make_call_num.test 10)

let aux_texte () = Exec.main "texte.ml"






let f = ref false
let interp = ref false
let num = ref false

let options = [
  "-f", Arg.Set f, ": fonctions_de_f";
  "-num", Arg.Set num, ": appel_numero";
  "-interp", Arg.Set interp, ": interprete aux_texte.ml";

]


let () = Arg.parse options (fun _ -> ()) ""



let () = 
  if !f then fonctions_de_f ();
  if !num then appel_numero ();
  if !interp then aux_texte ()

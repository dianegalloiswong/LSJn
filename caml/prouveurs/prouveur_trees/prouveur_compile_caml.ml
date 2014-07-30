let main (formule : Def.formule) : bool =
  if Options.affiche_temps_etapes() then Format.printf "compilation vers caml : %!";
  Time.time Compile_caml_direct.main formule;
  Use_ocamlc.main ()

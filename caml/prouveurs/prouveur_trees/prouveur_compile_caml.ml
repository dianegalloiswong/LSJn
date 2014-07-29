let main (f : Def.formule) : bool =

  Format.printf "compilation vers caml : %!";
  Time.time (fun () ->

    Init_sf_classe.main f;
    Init_priorite.main ();

    Cote.remplir ();
    Make_fonctions_sf.main ();
    Make_call_num.main ();

    Compile_caml_direct.main ()

  ) ();

  Use_ocamlc.main ()

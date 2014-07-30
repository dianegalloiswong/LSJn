let main f =
  Init_sf_classe.main f;
  Init_priorite.main ();

  Cote.remplir ();
  Make_fonctions_sf.main ();
  Make_call_num.main ()

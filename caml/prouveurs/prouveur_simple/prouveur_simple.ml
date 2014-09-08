let main formule =
(*
  Init_sf_classe.main formule;
  Init_priorite.main ();
*)
  Indexation.main formule;

  let m = (Array.length !Global_ref.sf) - 1 in
  Seq.of_sous_formule m;

  Prouvable.prouvable ()


let main_CL formule =
  Init_sf_classe.main formule;
  Init_priorite.main ();

  let m = (Array.length !Global_ref.sf) - 1 in
  Seq.of_sous_formule_CL m;

  Prouvable.prouvable ()




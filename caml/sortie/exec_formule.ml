let prouveur () = 
  if Options.prouveur_trees() then 
    Prouveur_trees.main 
  else if !Options.compile_caml then
    Prouveur_compile_caml.main
  else
    Prouveur_caml.main

let bool_to_string_fr b = if b then "vrai" else "faux"

let un_prouveur (f,att_opt,nom) =
  if nom<>"" && Options.affiche_nom_formule() then Format.printf "%s@." !Path.nom;
  if Options.affiche_formule() then Format.printf "%s@." (To_string.formule f);
  if !Options.indexation then (Indexation.main f; Indexation.print ())
  else 
    begin
    try
      Time.time (fun () ->
	let rep = (prouveur ()) f in
	if Options.affiche_rep() then Rep.print rep (*Format.printf "%s@." (bool_to_string_fr rep)*);
	match att_opt with
	  | None -> ()
	  | Some batt ->
	    let b = Rep.est_vrai rep in
	    if b <> batt then
	      Format.printf "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n!! %s\n!! obtenu : %s    attendu : %s\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!@." !Path.nom (bool_to_string_fr b) (bool_to_string_fr batt)
       ) ()
    with Time.Timeout -> ()
    end;
  if not !Options.rien_afficher then Format.printf "@."
  



let all arg =
  
  Options.trees:=false;
  Options.trees_machine:=false;
  Options.compile_caml:=false;

  (* prouveur simple *)
  un_prouveur arg;

  (* prouveur compile-caml *)
  Options.compile_caml:=true;
  un_prouveur arg;
  Options.compile_caml:=false;

  (* prouveur trees *)
  Options.trees:=true;
  un_prouveur arg;
  Options.trees:=false;

  (* prouveur trees-machine *)
  Options.trees_machine:=true;
  un_prouveur arg;
  Options.trees_machine:=false;

  Test_all.nom := !Path.nom;
  Test_all.print_res_formule ()


let main ((f,_,nom) as arg) =
  Path.set_nom nom; Path.formule:=Some f;
  if !Options.all then all arg else un_prouveur arg;
  Path.reset ()

(*
let bts b = if b then "vrai" else "faux"

let test attopt f =
  if Options.print_formule() then Format.printf "%s@." (To_string.formule f);
  if !Options.indexation then Init_sf_classe.test f;
  Time.time (fun () ->
  try
  let rep = main f in
  if Options.print_rep() then Rep.print rep;
  match attopt with
    | None -> ()
    | Some (batt,_) ->
      let b = vrai rep in
      if b <> batt then
	Format.printf "!!!!!!!!!!!!!!!!!    obtenu : %s    attendu : %s@." (bts b) (bts batt)
  with Time.Temps_ecoule -> () (*Format.printf "temps écoulé (%fs)@." !Options.temps_max*)
  ) ()
*)
(*
let test f =
  Format.printf "%s@." (To_string.formule f);
  Format.printf "IL: ";
  let rep = Options.time (main IL) f in
  print_rep rep;
  if !Options.classique then
    (Format.printf "CL: ";
     print_rep (main CL f))


let test_attendu (f,b1,b2) =
  if !Options.classique then
  begin
  let repIL = main IL f and repCL = main CL f in
  let resIL = vrai repIL and resCL = vrai repCL in
  if resIL=b1 && resCL=b2 then
    if !Options.compare_seul then (*Format.printf "OK.@."*)()
    else
      (Format.printf "%s@." (To_string.formule f);
       Format.printf "IL: ";
       print_rep repIL;
       Format.printf "CL: ";
       print_rep repCL;
       Format.printf "OK.@.")
  else 
    Format.printf "%s@.Résultats obtenus :    IL : %b   CL : %b@.Résultats attendus :   IL : %b   CL : %b@." (To_string.formule f) resIL resCL b1 b2
  end

  else
  begin
  let repIL = Options.time (main IL) f in
  let resIL = vrai repIL in
  if resIL=b1 then
    if !Options.compare_seul then (*Format.printf "OK.@."*)()
    else
      (Format.printf "%s@." (To_string.formule f);
       Format.printf "IL: ";
       print_rep repIL;
       Format.printf "OK.@.")
  else 
    Format.printf "%s@.Résultats obtenus :    IL : %b@.Résultats attendus :   IL : %b   CL : %b@." (To_string.formule f) resIL b1 b2
  end

    *)




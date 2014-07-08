
let bool_to_string_fr b = if b then "vrai" else "faux"

let main att_opt f =
  if Options.print_formule() then Format.printf "%s@." (To_string.formule f);
  if !Options.indexation then (Init_sf_classe.test f)
  else 
    (try
       Time.time (fun () ->
	 let rep = Prouveur.main f in
	 if Options.print_rep() then (*Rep.print rep*) Format.printf "%s@." (bool_to_string_fr rep);
	 match att_opt with
	   | None -> ()
	   | Some (batt,_) ->
	     let b = (*Rep.vrai*) rep in
	     if b <> batt then
	       Format.printf "!!!!!!!!!!!!!!!!!    obtenu : %s    attendu : %s@." (bool_to_string_fr b) (bool_to_string_fr batt)
       ) ()
     with Time.Temps_ecoule -> () )
  ; Format.printf "@."






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




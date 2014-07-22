open Def
open Ast_ILTP



let traite_attendus l = Some (List.fold_left (fun (b1,b2) -> function IL b -> (b,b||b2) | CL b -> (b1,b)) (false,false) l)


let traite_fact fopt = function
  | Conj (s,f) ->
    if Options.affiche_ax_et_conj() then Format.printf "Conjecture %s : %s@." s (To_string.formule f);
    (match fopt with None -> Some f | Some ax -> Some (F(Imp,ax,f)))
  | Ax (s,f) ->
    if Options.affiche_ax_et_conj() then Format.printf "Axiome %s : %s@." s (To_string.formule f);
    (match fopt with None -> Some f | Some ax -> Some (F(Et,ax,f)))
  | Autre (_,s,_) ->
    Format.printf "%s inconnu : fichier non traite@." s;
    raise Exit

let traite_facts attopt l =
  let fopt = try List.fold_left traite_fact None l with Exit -> None in
  match fopt with None -> () | Some f ->
    Exec_formule.main attopt f

let localisation nom pos =
  let l = pos.Lexing.pos_lnum in
  let c = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
  Format.eprintf "File \"%s\", line %d, characters %d-%d:@." nom l (c-1) c

let parse nom =
  let f = open_in nom in 
  let buf = Lexing.from_channel f in
  try
    let fichier = Parser_ILTP.fichier Lexer_ILTP.token buf in
    close_in f;
    fichier
  with
    | Lexer_ILTP.Lexing_error c -> 
      localisation nom (Lexing.lexeme_start_p buf);
      Format.eprintf "Erreur dans l'analyse lexicale: %c@." c;
      raise Exit
    | Parser_ILTP.Error -> 
      localisation nom (Lexing.lexeme_start_p buf);
      Format.eprintf "Erreur dans l'analyse syntaxique@.";
      raise Exit
    | _ ->
      Format.eprintf "Erreur dans Analyseur.parse@.";
      raise Exit

let main nom =
  if Options.affiche_nom_fichier() then Format.printf "%s@." nom;
  try
    let attendus,facts = parse nom in
    let attopt = traite_attendus attendus in
    traite_facts attopt facts;
    (*if Options.print_fichier() then Format.printf "@."*)
  with Exit -> ()


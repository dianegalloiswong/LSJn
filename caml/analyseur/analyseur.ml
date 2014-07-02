open Def
open Ast



let traite_attendus l = List.fold_left (fun (b1,b2) -> function IL b -> (b,b||b2) | CL b -> (b1,b)) (false,false) l

(*
let traite_fact (b1,b2) = function
  | Conj (s,f) ->
    if not !Options.compare_seul then Format.printf "Conjecture %s :" s;
    if !Options.compare then 
      LSJn.test_attendu (f,b1,b2)
    else
      LSJn.test f
  | Autre (s,s2,_) ->
    Format.printf "%s pas conjecture mais %s : fichier non traite@." s s2;
    raise Exit

let traite_facts att l = try List.iter (traite_fact att) l with Exit -> ()
*)

let traite_fact fopt = function
  | Conj (s,f) ->
    if not !Options.compare_seul then Format.printf "Conjecture %s : %s@." s (To_string.formule f);
    (match fopt with None -> Some f | Some ax -> Some (F(Imp,ax,f)))
  | Ax (s,f) ->
    if not !Options.compare_seul then Format.printf "Axiome %s : %s@." s (To_string.formule f);
    (match fopt with None -> Some f | Some ax -> Some (F(Et,ax,f)))
  | Autre (_,s,_) ->
    Format.printf "%s inconnu : fichier non traite@." s;
    raise Exit

let traite_facts (b1,b2) l =
  let fopt = try List.fold_left traite_fact None l with Exit -> None in
  match fopt with None -> () | Some f ->
  if !Options.compare then 
    LSJn.test_attendu (f,b1,b2)
  else
    LSJn.test f

let localisation nom pos =
  let l = pos.Lexing.pos_lnum in
  let c = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
  Format.eprintf "File \"%s\", line %d, characters %d-%d:@." nom l (c-1) c

let parse nom =
  let f = open_in nom in 
  let buf = Lexing.from_channel f in
  try
    let fichier = Parser.fichier Lexer.token buf in
    close_in f;
    fichier
  with
    | Lexer.Lexing_error c -> 
      localisation nom (Lexing.lexeme_start_p buf);
      Format.eprintf "Erreur dans l'analyse lexicale: %c@." c;
      raise Exit
    | Parser.Error -> 
      localisation nom (Lexing.lexeme_start_p buf);
      Format.eprintf "Erreur dans l'analyse syntaxique@.";
      raise Exit
    | _ ->
      Format.eprintf "Erreur dans Analyseur.parse@.";
      raise Exit
       


let main_f nom =
  Format.printf "%s@." nom;
  try
    let attendus,facts = parse nom in
    let att = traite_attendus attendus in
    traite_facts att facts
  with Exit -> ()


let rec main_d dnom =
  let dh = Unix.opendir dnom in
  try
    while true do
      let s = Unix.readdir dh in
      if s.[0] <> '.' then main (dnom^"/"^s)
    done;
  with End_of_file -> ()

and main nom =
    match (Unix.stat nom).Unix.st_kind with
      | Unix.S_REG -> main_f nom
      | Unix.S_DIR -> main_d nom
      | _ -> ()

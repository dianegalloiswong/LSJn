open Ast_pos_trees
open Tree

let variables = Hashtbl.create 17
let fonctions = Hashtbl.create 17

let var (x,_) = Format.printf "variable_%s" x
let func (f,_) = Format.print_string (if Hashtbl.mem fonctions f then "fonctions"^f else "inexistant")

let rec expr e = match (fst e) with
  | EVar x -> Format.printf "expr_var tt "; var x
  | ENull -> Format.printf "expr_null tt"
  | EInt n -> Format.printf "expr_int tt %d" n (*?*)
  | ENode (e1,e2) ->
    Format.printf "expr_pair tt (";
    expr e1;
    Format.printf ") (";
    expr e2;
    Format.printf ")"
  | EMatch (e1,x,y,e2) ->
    Format.printf "expr_match tt "; var x; Format.printf " "; var y;
    Format.printf " (";
    expr e1;
    Format.printf ") (\n";
    expr e2;
    Format.printf ")"
  | ELetin (x,e1,e2) ->
    Format.printf "expr_let tt ";
    var x;
    Format.printf " (";
    expr e1;
    Format.printf ") (\n";
    expr e2;
    Format.printf ")"
  | ECall (f,e) ->
    Format.printf "expr_app tt ";
    func f;
    Format.printf " (";
    expr e;
    Format.printf ")"
  | EIsnull e ->
    Format.printf "expr_isnull tt (";
    expr e;
    Format.printf ")"
  | EIsint e ->
    Format.printf "expr_isint tt (";
    expr e;
    Format.printf ")"
  | ELeq (e1,e2) ->
    Format.printf "expr_leq tt (" ;
    expr e1;
    Format.printf ") (";
    expr e2;
    Format.printf ")"
 | EIf (b,e1,e2) ->
    Format.printf "expr_ite tt (";
    expr b;
    Format.printf ") (\n";
    expr e1;
    Format.printf ") (\n";
    expr e2;
    Format.printf ")"

  (***)

  | EIsnode e ->
    expr (
      EIf(EIsnull e,EInt 0,EIf(EIsint e,EInt 0,EInt 1))
    ,snd e)
  | ELess (e1,e2) ->
    expr (
      EIf(ELeq(e1,e2),EIf(ELeq(e2,e1),EInt 0,EInt 1),EInt 0)
    ,snd e)
  | EEq (e1,e2) ->
    expr (
      EAnd(ELeq(e1,e2),ELeq(e2,e1))
    ,snd e)
  | ESucc e ->
    Format.printf "(Int (to_int ";
    expr e;
    Format.printf "+1))"
  | ENot e ->
    expr (
      EIf(e,EInt 0,EInt 1)
    ,snd e)
  | EAnd (e1,e2) ->
    expr (
      EIf(e1,e2,EInt 0)
    ,snd e)
  | EOr (e1,e2) ->
     expr (
      EIf(e1,EInt 1,e2)
     ,snd e)

let decl_func (f,x,e) =
  Format.printf "and ";
  func f;
  Format.printf " ";
  var x;
  Format.printf " = \n";
  expr e;
  Format.printf "@."

let prog ((l,e),nom) =
  Format.printf "\n(* compilé à partir du fichier %s *)@." nom;
  Hashtbl.clear fonctions;
  List.iter (fun ((f,_),_,_) -> Hashtbl.add fonctions f ()) l;
  List.iter decl_func l;
  Format.printf "\n let expr =";
  expr e;
  Format.printf "\nlet () = Format.printf \"%%s@@.\" (match expr with Int 0 -> \"0\" | Int _ -> \"1\" | _ -> \"E\")";
  Format.print_flush ()



let cat_entete = "cat "^Path.entete_caml^" > "^Path.code_caml

let main (p : Ast_pos_trees.prog) =

  ignore (Unix.system cat_entete);

  let fd = Unix.openfile Path.code_coq [Unix.O_WRONLY;Unix.O_CREAT] 0o640 in
  ignore (Unix.lseek fd 0 Unix.SEEK_END);
  let out = Unix.out_channel_of_descr fd in
  Format.set_formatter_out_channel out;

  prog p;

  Format.set_formatter_out_channel stdout;
  Unix.close fd

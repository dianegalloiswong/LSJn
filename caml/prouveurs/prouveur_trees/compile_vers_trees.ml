open Ast_trees
open Tree

let rec expr = function
  | EVar s -> Format.printf "%s" s
  | ENull -> Format.printf "null"
  | EInt n -> Format.printf "%d" n
  | ENode (e1,e2) ->
    Format.printf "< ";
    expr e1;
    Format.printf " , ";
    expr e2;
    Format.printf " >"
  | EMatch (e1,x,y,e2) ->
    Format.printf "(match ";
    expr e1;
    Format.printf " with <%s,%s> => @." x y;
    expr e2;
    Format.printf ")"
  | ELetin (x,e1,e2) ->
    Format.printf "(let %s = " x;
    expr e1;
    Format.printf " in @.";
    expr e2;
    Format.printf ")"
  | ECall (f,e) ->
    Format.printf "(call %s " f;
    expr e;
    Format.printf ")"
  | EIsnull e ->
    Format.printf "(isnull ";
    expr e;
    Format.printf ")"
  | EIsint e ->
    Format.printf "(isint ";
    expr e;
    Format.printf ")"
  | EIsnode e ->
    Format.printf "(isnode ";
    expr e;
    Format.printf ")"
  | ELeq (e1,e2) ->
    Format.printf "(" ;
    expr e1;
    Format.printf "<=";
    expr e2;
    Format.printf ")"
  | ELess (e1,e2) ->
    Format.printf "(" ;
    expr e1;
    Format.printf "<";
    expr e2;
    Format.printf ")"
  | EIf (b,e1,e2) ->
    Format.printf "(if ";
    expr b;
    Format.printf " then @.";
    expr e1;
    Format.printf " else @.";
    expr e2;
    Format.printf ")"




  | EEq (e1,e2) ->
    Format.printf "(" ;
    expr e1;
    Format.printf "=";
    expr e2;
    Format.printf ")"
  | ESucc e ->
    Format.printf "(";
    expr e;
    Format.printf " +1)"
  | EPred e ->
    Format.printf "(";
    expr e;
    Format.printf " -1)"
  | ENot e ->
    Format.printf "not (";
    expr e;
    Format.printf ")"
  | EAnd (e1,e2) ->
    Format.printf "(" ;
    expr e1;
    Format.printf "&&";
    expr e2;
    Format.printf ")"
  | EOr (e1,e2) ->
    Format.printf "(" ;
    expr e1;
    Format.printf "||";
    expr e2;
    Format.printf ")"


let decl_func (f,x,e) =
  Format.printf "let %s %s = @." f x;
  expr e;
  Format.printf "@."

(*
let prog (l,e) =
  List.iter decl_func l;
  Format.printf "in@.";
  expr e
*)


(***)



let string_cat () =
  (List.fold_left (fun s s1 -> s^" "^s1) "cat" Path.liste_code_trees_fixe)
    ^" >> "^Path.code_trees()

let main f =
  let fd = Unix.openfile (*fonctions_sf*)(Path.code_trees()) [Unix.O_WRONLY;Unix.O_CREAT;Unix.O_TRUNC] 0o640 in
  let out = Unix.out_channel_of_descr fd in
  Format.set_formatter_out_channel out;

  Format.printf "\n(* fonctions dépendant de la formule *)\n";
  Format.printf "\n(*\n%s@." (To_string.formule f);

(*
  Init_sf_classe.test f;
  Init_priorite.main ();
  Cote.remplir ();
*)
  Indexation.main f;
  Indexation.print ();

  Make_fonctions_sf.main ();
  Make_call_num.main ();

  Format.printf "*)\n@.";

  List.iter (fun df -> decl_func df;Format.printf"@.") (List.rev !Fonctions_compilees.fonctions);

  Format.printf "\n(****************************************)\n\n(* code fixe *)\n@.";

  Format.set_formatter_out_channel stdout;
  Unix.close fd;

  ignore (Unix.system (string_cat()))

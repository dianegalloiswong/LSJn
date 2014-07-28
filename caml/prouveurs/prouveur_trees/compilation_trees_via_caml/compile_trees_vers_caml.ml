open Ast_pos_trees
open Tree

let fonctions = Hashtbl.create 17

let var (x,_) = Format.printf "variable_%s" x
let func (f,_) = Format.print_string (if Hashtbl.mem fonctions f then "fonctions"^f else "inexistant")

let rec expr e = match (fst e) with
  | EVar x -> var x
  | ENull -> Format.printf "Null"
  | EInt n -> Format.printf "(Int %d)" n
  | ENode (e1,e2) ->
    Format.printf "(Node ( ";
    expr e1;
    Format.printf " , ";
    expr e2;
    Format.printf " ))"
  | EMatch (e1,x,y,e2) ->
    Format.printf "(match ";
    expr e1;
    Format.printf " with Node(";
    var x;
    Format.printf ",";
    var y;
    Format.printf ") -> \n";
    expr e2;
    Format.printf "\n|_->assert false)\n"
  | ELetin (x,e1,e2) ->
    Format.printf "(let ";
    var x;
    Format.printf " = ";
    expr e1;
    Format.printf " in @.";
    expr e2;
    Format.printf ")"
  | ECall (f,e) ->
    Format.printf "(";
    func f;
    Format.printf " ";
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
    Format.printf "(of_bool(to_int " ;
    expr e1;
    Format.printf " <= to_int ";
    expr e2;
    Format.printf "))"
  | ELess (e1,e2) ->
    Format.printf "(of_bool(to_int " ;
    expr e1;
    Format.printf " < to_int ";
    expr e2;
    Format.printf "))"
  | EIf (b,e1,e2) ->
    Format.printf "(if to_bool ";
    expr b;
    Format.printf " then \n";
    expr e1;
    Format.printf " else \n";
    expr e2;
    Format.printf ")"

  | EEq (e1,e2) ->
    Format.printf "(of_bool(to_int " ;
    expr e1;
    Format.printf " = to_int ";
    expr e2;
    Format.printf "))"
  | ESucc e ->
    Format.printf "(Int (to_int ";
    expr e;
    Format.printf "+1))"
  | ENot e ->
    Format.printf "(of_bool(not(to_bool ";
    expr e;
    Format.printf ")))"
  | EAnd (e1,e2) ->
    Format.printf "(of_bool(to_bool " ;
    expr e1;
    Format.printf " && to_bool ";
    expr e2;
    Format.printf "))"
  | EOr (e1,e2) ->
    Format.printf "(of_bool(to_bool " ;
    expr e1;
    Format.printf " || to_bool ";
    expr e2;
    Format.printf "))"

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



let cat_entete = "cat "^Path.entete_caml^" > "^Path.code_caml()

let main (p : Ast_pos_trees.prog) =

  ignore (Unix.system cat_entete);

  let fd = Unix.openfile (Path.code_caml()) [Unix.O_WRONLY;Unix.O_CREAT] 0o640 in
  ignore (Unix.lseek fd 0 Unix.SEEK_END);
  let out = Unix.out_channel_of_descr fd in
  Format.set_formatter_out_channel out;

  prog p;

  Format.set_formatter_out_channel stdout;
  Unix.close fd

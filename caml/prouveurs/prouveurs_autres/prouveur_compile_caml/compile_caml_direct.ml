open Ast_trees

(* attention : sans les positions ! *)

let fonctions = Hashtbl.create 17

let var x = (*Format.printf "var_%s" x*) Format.print_string x
let func f = Format.print_string (
  if not (String.sub f 0 2 = "sf") || Hashtbl.mem fonctions f then 
    (*"func_"^*)f 
  else "inexistant"
)

let rec expr = function
  | EVar x -> var x
  | EInt n -> Format.printf "%d" n
  | ENode (e1,e2) ->
    Format.printf "(";
    expr e1;
    Format.printf ",";
    expr e2;
    Format.printf ")"
  | ELetin (x,e1,e2) ->
    Format.printf "let ";
    var x;
    Format.printf " = ";
    expr e1;
    Format.printf " in\n";
    expr e2
  | ECall (f,e) ->
    Format.printf "(";
    func f;
    Format.printf " ";
    expr e;
    Format.printf ")"
  | ESucc e ->
    Format.printf "((";
    expr e;
    Format.printf ")+1)"
  | ELess (e1,e2) ->
    expr e1;
    Format.printf " < ";
    expr e2
  | EIf (b,e1,e2) ->
    Format.printf "(if ";
    expr b;
    Format.printf " then \n";
    expr e1;
    Format.printf " else \n";
    expr e2;
    Format.printf ")"
  | _ -> assert false

let rec expr_seq = function
  | ELetin ("seq",e1,e2) ->
    expr_seq e1;
    Format.printf ";\n";
    expr_seq e2
  | ENode (e,EVar "seq") -> expr_seq e
  | EVar "seq" -> Format.printf "()\n"
  | EVar x -> var x
  | EInt n -> Format.printf "%d" n
  | ENode (e1,e2) ->
    Format.printf "(";
    expr_seq e1;
    Format.printf ",";
    expr_seq e2;
    Format.printf ")"
  | ELetin (x,e1,e2) ->
    Format.printf "let ";
    var x;
    Format.printf " = ";
    expr_seq e1;
    Format.printf " in\n";
    expr_seq e2
  | ECall (f,e) ->
    Format.printf "(";
    func f;
    Format.printf " ";
    expr_seq e;
    Format.printf ")"
  | ESucc e ->
    Format.printf "((";
    expr_seq e;
    Format.printf ")+1)"
  | _ -> assert false


let decl_func (f,x,e) =
  Hashtbl.add fonctions f ();
  Format.printf "and ";
  func f;
  begin
  match x with
    | "seq" -> (*fonction ajout_formule_initiale*)
      Format.printf " () = \n";
      expr_seq e
    | "arg" ->
      if String.sub f 0 2 = "sf" then
	begin
        Format.printf " "; var "i"; Format.printf " = \n";
	match e with EMatch(e1,x,y,e2) ->
        assert (x = "i" && y = "seq");
        expr_seq e2 
	|_->assert false
	end
      else if String.sub f 0 5 = "call_" then
	begin
        Format.printf " (k,x) = \n";
        match e with EMatch(e1,x,y,e2) ->
        assert (x = "k" && y = "x");
        expr e2
        |_->assert false
	end
    | _ -> assert false
  end;
  Format.printf "\n"



let cat_entete () = "cat "^Path.entete_caml_direct^" >> "^Path.code_caml_direct()

let from_fonctions_compilees () =

  (*(match !Path.formule with None -> () | Some f -> Format.printf "\n(* formule : %s *)@." (To_string.formule f));*)

  

  let fd = Unix.openfile (Path.code_caml_direct()) [Unix.O_WRONLY;Unix.O_CREAT;Unix.O_TRUNC] 0o640 in
  (*ignore (Unix.lseek fd 0 Unix.SEEK_END);*)
  let out = Unix.out_channel_of_descr fd in
  Format.set_formatter_out_channel out;

  (match !Path.formule with None -> () | Some f -> Format.printf "\n(* formule : %s *)@." (To_string.formule f));

  ignore (Unix.system (cat_entete()));
  ignore (Unix.lseek fd 0 Unix.SEEK_END);

  Format.printf "\n(* compilé à partir de la formule *)@.";

  Hashtbl.clear fonctions;
  List.iter (fun df -> decl_func df;Format.printf"@.") (List.rev !Fonctions_compilees.fonctions);

  Format.printf "\nlet expr = programme ()\n";
  Format.printf "\nlet () = Format.printf \"%%s@@.\" (if expr then \"1\" else \"0\")";
  Format.printf "\nlet () = Format.printf \"%%d@@.\" !appels";
  Format.printf "@.";

  Format.set_formatter_out_channel stdout;
  Unix.close fd



let main f = 
  (*Init_fonctions_compilees.main f;*)
  Indexation.main f;
  Make_fonctions_sf.main ();
  Make_call_num.main ();

  from_fonctions_compilees ()

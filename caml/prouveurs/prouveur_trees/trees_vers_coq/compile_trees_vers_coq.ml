open Ast_pos_trees
open Tree

let prefixe = "
Require Import Arith.
Require Import List.
Require Import String.

Require Import env.

Require Import fun_defs.
Require Import fam.
Require Import fam_eval.
Require Import fam_compiler.
Require Import fam_print.
"
let suffixe = "
Let code := compile_code prouveur_prog (CALL func_programme NULL).

Eval compute in fam_linstr_string code.
"


let variables_liste = [
  "arg"; "arg1"; "arg2";
  "a"; "b"; "c"; "h"; "i"; "j"; "k"; "l"; "n"; "p"; "t"; "x"; "y";
  "x1"; "x2"; "y1"; "y2"; "n1";
  "hd"; "tl";
  "nplus1"; "nmoins1";
  "seq"; "formules"; "G"; "D"; "reste"; "classes"; "CG"; "CD"; "infos"; "axiomes"; "fauxL"; "id";
  "sf"; "cl"; "i_sf";
  "ac"; "ic"; "sfc"; "pc"; "courant";
  "rep"; "rep1"; "rep2"; "rep3"; "fini";
  "nb_g"; "nb_d"; "nbs";
]
(*
let variables = Hashtbl.create 7
let () = ignore (List.fold_left (fun i x ->
  Hashtbl.add variables x i; i+1
) 0 variables_liste)
*)
let print_decl_variables () = 
  ignore (List.fold_left (fun i x ->
    Format.printf "Let var_%s := in_Var %d.\n" x i; i+1
  ) 0 variables_liste)


let fonctions = Hashtbl.create 17

let print_decl_noms_func () =
  Format.printf "Definition fonction_inexistante := in_Fun 0.\n";
  ignore (Hashtbl.fold (fun f () i ->
    Format.printf "Definition func_%s := in_Fun %d.\n" f i; i+1
  ) fonctions 1) (* 0 réservé pour fonction inexistante *)


let var (x,_) = 
if not (List.mem x variables_liste) then Format.eprintf "trees vers coq : variable %s non répertoriée@." x;
Format.printf "var_%s" x
let func (f,_) = Format.print_string (if Hashtbl.mem fonctions f then "func_"^f else "fonction_inexistante")




let pos = Lexing.dummy_pos,Lexing.dummy_pos
let eint n = (EInt n,pos)
let eisnull e = (EIsnull e,pos)
let eisint e = (EIsint e,pos)
let eleq (e1,e2) = (ELeq(e1,e2),pos)
let eif (b,e1,e2) = (EIf(b,e1,e2),pos)
let eand (e1,e2) = (EAnd(e1,e2),pos)
let ecall (s,e) = (ECall((s,pos),e),pos)

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
 | ESucc e ->
   Format.printf "expr_succ tt (";
   expr e;
   Format.printf ")"

  (***)

  | EIsnode e ->
    expr (
      eif(eisnull e,eint 0,eif(eisint e,eint 0,eint 1))
    )
  | ELess (e1,e2) ->
    expr (
      eif(eleq(e1,e2),eif(eleq(e2,e1),eint 0,eint 1),eint 0)
    )
  | EEq (e1,e2) ->
    expr (
      eand(eleq(e1,e2),eleq(e2,e1))
    )

  | ENot e ->
    expr (
      eif(e,eint 0,eint 1)
    )
  | EAnd (e1,e2) ->
    expr (
      eif(e1,e2,eint 0)
    )
  | EOr (e1,e2) ->
     expr (
      eif(e1,eint 1,e2)
     )

let decl_func_body (f,x,e) =
  Format.printf "Let body_"; func f; Format.printf " : expr unit :=\n"; expr e; Format.printf ".\n@."

let decl_func (f,x,e) =
  Format.printf "("; func f; Format.printf ", ("; var x; Format.printf ",body_"; func f;
  Format.printf ",tt))::\n"


let prog ((l,e),nom) =

  Format.printf "\n(* compilé à partir du fichier %s *)\n" nom;
  (match !Path.formule with None -> () | Some f -> Format.printf "\n(* formule : %s *)@." (To_string.formule f));

  Hashtbl.clear fonctions;
  List.iter (fun ((f,_),_,_) -> Hashtbl.add fonctions f ()) l;

(*  Format.printf "
Definition recfun := Fun*(Var*expr X*X).\n
Definition program := list recfun.\n
";*)

  Format.print_string prefixe;

  Format.printf "@.";
  print_decl_noms_func ();
  Format.printf "@.";
  print_decl_variables ();
  Format.printf "@.";

  List.iter decl_func_body l;

  Format.printf "Definition prouveur_prog : program unit :=\n";
  List.iter decl_func l;
  Format.printf "nil.@.";

  Format.print_string suffixe;

  Format.printf "@."



let main (p : Ast_pos_trees.prog) =

  let fd = Unix.openfile (Path.code_coq()) [Unix.O_WRONLY;Unix.O_CREAT;Unix.O_TRUNC] 0o640 in
  let out = Unix.out_channel_of_descr fd in
  Format.set_formatter_out_channel out;

  prog p;

  Format.set_formatter_out_channel stdout;
  Unix.close fd

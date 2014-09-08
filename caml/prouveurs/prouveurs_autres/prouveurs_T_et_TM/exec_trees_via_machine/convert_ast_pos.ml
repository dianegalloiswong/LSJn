open Ast_trees


let pos = Lexing.dummy_pos,Lexing.dummy_pos
let eint n = (Ast_pos_trees.EInt n,pos)
let eisnull e = (Ast_pos_trees.EIsnull e,pos)
let eisint e = (Ast_pos_trees.EIsint e,pos)
let eleq (e1,e2) = (Ast_pos_trees.ELeq(e1,e2),pos)
let eif (b,e1,e2) = (Ast_pos_trees.EIf(b,e1,e2),pos)
let eand (e1,e2) = (Ast_pos_trees.EAnd(e1,e2),pos)
let ecall (s,e) = (Ast_pos_trees.ECall((s,pos),e),pos)

let var x = fst x
let func f = fst f

let rec expr e = match (fst e) with
  | Ast_pos_trees.EVar x -> EVar (var x)
  | Ast_pos_trees.ENull -> ENull
  | Ast_pos_trees.EInt n -> EInt n
  | Ast_pos_trees.ENode (e1,e2) -> ENode (expr e1,expr e2)
  | Ast_pos_trees.EMatch (e1,x,y,e2) -> EMatch (expr e1,var x,var y,expr e2)
  | Ast_pos_trees.ELetin (x,e1,e2) -> ELetin (var x,expr e1,expr e2)
  | Ast_pos_trees.ECall (f,e) -> ECall (func f,expr e)
  | Ast_pos_trees.EIsnull e -> EIsnull (expr e)
  | Ast_pos_trees.EIsint e -> EIsint (expr e)
  | Ast_pos_trees.ELeq (e1,e2) -> ELeq (expr e1,expr e2)
  | Ast_pos_trees.EIf (b,e1,e2) -> EIf (expr b,expr e1,expr e2)
  | Ast_pos_trees.ESucc e -> ESucc (expr e)
 (* | Ast_pos_trees.EPred e -> EPred (expr e)*)

  (***)

  | Ast_pos_trees.EIsnode e ->
    expr (
      eif(eisnull e,eint 0,eif(eisint e,eint 0,eint 1))
    )
  | Ast_pos_trees.ELess (e1,e2) ->
    expr (
      eif(eleq(e1,e2),eif(eleq(e2,e1),eint 0,eint 1),eint 0)
    )
  | Ast_pos_trees.EEq (e1,e2) ->
    expr (
      eand(eleq(e1,e2),eleq(e2,e1))
    )
  | Ast_pos_trees.ENot e ->
    expr (
      eif(e,eint 0,eint 1)
    )
  | Ast_pos_trees.EAnd (e1,e2) ->
    expr (
      eif(e1,e2,eint 0)
    )
  | Ast_pos_trees.EOr (e1,e2) ->
    expr (
      eif(e1,eint 1,e2)
    )

let decl_func (f,x,e) = (func f,(var x,expr e))

let prog (ldf,e) = (List.map decl_func ldf,expr e)

let main = prog

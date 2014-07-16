type position = Lexing.position
type positions = position*position
type 'a pos = 'a * positions



type var = string pos
type func = string pos

type expr0 =
  | EVar of var
  | ENull
  | EInt of int
  | ENode of expr*expr
  | EMatch of expr*var*var*expr
  | ELetin of var*expr*expr
  | ECall of func*expr
  | EIsnull of expr
  | EIsint of expr
  | EIsnode of  expr
  | ELeq of expr*expr
  | EIf of expr*expr*expr

  (*nouveaux*)
  | EEq of expr*expr (* égalité structurelle *)
  | ELess of expr*expr
  | ESucc of expr
  | EAnd of expr*expr

and expr = expr0 pos

type decl_func = func*var*expr

type prog = (decl_func list)*expr



(*
let evar s = EVar s
let eint n = EInt n
let efalse = EInt 0
let etrue = EInt 1
let enode e1 e2 = ENode (e1,e2)
let ematch e1 x y e2 = EMatch (e1,x,y,e2)
let eletin x e1 e2 = ELetin (x,e1,e2)
let ecall f e = ECall (f,e)
let eisnull e = EIsnull e
let eisint e = EIsint e
let eisnode e = EIsnode e
let eleq e1 e2 = ELeq (e1,e2)
let eless e1 e2 = ELess (e1,e2)
let eif b e1 e2 = EIf (b,e1,e2)
let eeq e1 e2 = EEq (e1,e2)
let esucc e = ESucc e
*)

(*type tree = Null | Int of int | Node of tree*tree*)

type var = string
type func = string

type expr =
  | EVar of var
  | ENull
  | EInt of int
  | ENode of expr*expr
  | EMatch of expr*var*var*expr
  | ELetin of var*expr*expr
  | ECall of func*expr
  | EIsnull of expr
  | EIsint of expr
  | ELeq of expr*expr
  | EIf of expr*expr*expr
  | ESucc of expr
  | EPred of expr

  (*nouveaux*)
  | EEq of expr*expr (* égalité structurelle *)
  | EIsnode of  expr
  | ELess of expr*expr
  | ENot of expr
  | EAnd of expr*expr
  | EOr of expr*expr

type decl_func = func*var*expr

type prog = (decl_func list)*expr




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

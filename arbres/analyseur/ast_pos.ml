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
  | ENot of expr
  | EAnd of expr*expr
  | EOr of expr*expr

and expr = expr0 pos

type decl_func = func*var*expr

type prog = (decl_func list)*expr

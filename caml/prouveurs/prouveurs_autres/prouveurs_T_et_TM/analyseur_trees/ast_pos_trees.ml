type position = Lexing.position
type positions = position*position




type var = string *positions
type func = string *positions

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

  | ELeq of expr*expr
  | EIf of expr*expr*expr

  (*nouveaux*)
  | EIsnode of  expr
  | EEq of expr*expr (* égalité de deux entiers *)
  | ELess of expr*expr
  | ESucc of expr
  | ENot of expr
  | EAnd of expr*expr
  | EOr of expr*expr

and expr = expr0 *positions

type decl_func = func*var*expr

type prog0 = (decl_func list)*expr
type prog = prog0 *string (*nom du fichier*)

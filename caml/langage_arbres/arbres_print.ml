open Arbres_ast



let rec tree = function
  | Null -> Format.printf "null"
  | Int n -> Format.printf "%d" n
  | Node (t1,t2) ->
    Format.printf "< ";
    tree t1;
    Format.printf " , ";
    tree t2;
    Format.printf " >"


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
  | ELeq (e1,e2) ->
    Format.printf "(" ;
    expr e1;
    Format.printf "<=";
    expr e2;
    Format.printf ")"
  | EIf (b,e1,e2) ->
    Format.printf "(if ";
    expr b;
    Format.printf " then ";
    expr e1;
    Format.printf " else ";
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
  | EAnd (e1,e2) ->
    Format.printf "(" ;
    expr e1;
    Format.printf "&&";
    expr e2;
    Format.printf ")"


let decl_func (f,x,e) =
  Format.printf "let %s %s = @." f x;
  expr e;
  Format.printf "@."


let prog (l,e) =
  List.iter decl_func l;
  Format.printf "in@.";
  expr e





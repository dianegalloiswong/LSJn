open Ast_pos
open Tree


exception Error of (string*positions)


module Smap = Map.Make (String)
(*
module Env = struct
  (*type t = (tree list) Smap.t*)
  let empty = Smap.empty
  let add k x m = 
    let l = try Smap.find k m with Not_found -> []
    in Smap.add k x::l m
  let remove k m =
    let l = try List.tl (Smap.find k m) with Not_found | Failure "tl" -> []
    in Smap.add k l m
  let find k m = try List.hd (Smap.find k m) with Failure "hd" -> raise Not_found
end
*)

let fonctions = Hashtbl.create 17 (* func -> var*expr *)


let tree_of_bool b = Int (if b then 1 else 0)

let rec interp_expr env (expr,pos) = match expr with
  | EVar x -> (try Smap.find (fst x) env with Not_found -> raise (Error ("unbound variable "^(fst x), snd x)))
  | ENull -> Null
  | EInt n -> Int  n
  | ENode (e1,e2) ->
    let t1 = interp_expr env e1 in
    let t2 = interp_expr env e2 in
    Node (t1,t2)
  | EMatch (e1,x,y,e2) ->
    let t1 = interp_expr env e1 in
    begin
    match t1 with
      | Node (tx,ty) ->
	let env' = Smap.add (fst x) tx env in
	let env' = Smap.add (fst y) ty env' in
	interp_expr env' e2
      | _ -> raise (Error ("should be a node (get matched)", snd e1))
    end
  | ELetin (x,e1,e2) ->
    let t1 = interp_expr env e1 in
    let env' = Smap.add (fst x) t1 env in
    interp_expr env' e2
  | ECall (f,e) -> 
    let t = interp_expr env e in 
    let (arg,body) = try Hashtbl.find fonctions (fst f) 
      with Not_found -> raise (Error ("unknown function "^(fst f), snd f))
    in
    let env' = Smap.add arg t env in
    interp_expr env' body
  | EIsnull e -> 
    let t = interp_expr env e in 
    tree_of_bool (t=Null)
  | EIsint e -> 
    let t = interp_expr env e in 
    tree_of_bool (match t with Int _->true|_->false)
  | EIsnode e -> 
    let t = interp_expr env e in 
    tree_of_bool (match t with Node _->true|_->false)
  | ELeq (e1,e2) | EEq (e1,e2) | ELess (e1,e2) ->
    let t1 = interp_expr env e1 in
    let t2 = interp_expr env e2 in
    begin
    match t1 with
      | Int n1 ->
	begin
	match t2 with
	  | Int n2 ->
	    let b = match expr with
	      | ELeq _ -> n1<=n2
	      | EEq _ -> n1=n2
	      | ELess _ -> n1<n2
	      | _ -> assert false
	    in tree_of_bool b
	  | _ -> raise (Error ("should be an integer (comparison)", snd e2))
	end
      | _ -> raise (Error ("should be an integer (comparison)", snd e1))
    end
  | EIf (b,e1,e2) ->
    let t = interp_expr env b in
    begin
    match t with
      | Int 0 -> interp_expr env e2
      | Int _ -> interp_expr env e1
      | _ -> raise (Error ("should be a boolean (condition)", snd b))
    end

  | ESucc e ->
    let t = interp_expr env e in
    begin
    match t with
      | Int n -> Int (n+1)
      | _ -> raise (Error ("should be an integer (succ)", snd e))
    end
  | EAnd (e1,e2) ->
    let t1 = interp_expr env e1 in
    begin
    match t1 with
      | Int 0 -> Int 0
      | Int _ -> 
	let t2 = interp_expr env e2 in
	(match t2 with Int _ -> t2 
	  | _ -> raise (Error ("should be a boolean (argument of &&)", snd e2)) )
      | _ -> raise (Error ("should be a boolean (argument of &&)", snd e1))
    end





let interp_decl_func (f,arg,e) =
  Hashtbl.add fonctions (fst f) (fst arg,e)


let interp_prog (dflist,e) =
  List.iter interp_decl_func dflist;
  interp_expr Smap.empty e

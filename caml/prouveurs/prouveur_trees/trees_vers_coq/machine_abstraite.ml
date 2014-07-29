open Tree

type var = int

module Env = Map.Make(struct type t = var let compare = compare end)
type env = Tree.tree Env.t

type stack_cell = A of int | V of tree option
type stack = stack_cell list

(*type state = { instr : int; env : Tree.t option Env.t; stack : stack_cell list }*)
type state = int * env * stack


type instr =
  | LDA of int
  | LDC of int
  | LDN
  | LDV of var
  | SAVE of var
  | STV of var
  | RST of var
  | UJP
  | SKIP
  | PAIR
  | SPLIT
  | SWAP
  | ISNULL
  | ISINT
  | SUCC
  | PRED
  | LEQ
  | HALT

(* STOP = LDA 0::UJP::nil *)
exception Halt of int*stack

let step t_instr (n,e,s) = match t_instr.(n) with
  | LDA a -> (n+1,e,A a::s)
  | LDC v -> (n+1,e,V(Some(Int v))::s)
  | LDN -> (n+1,e,V(Some Null)::s)
  | LDV x -> (n+1,e,V(Some (Env.find x e))::s)
  | SAVE x -> let v = try Some(Env.find x e) with Not_found -> None in
	      (n+1,e,V v::s)
  | STV x -> (match s with V (Some t)::s ->
    (n+1,Env.add x t e,s)     |_->assert false)
  | RST x -> (match s with V v::s ->
    let e = match v with Some t -> Env.add x t e | None -> Env.remove x e in
    (n+1,e,s)     |_->assert false)
  | UJP -> (match s with A a::s ->
    (a,e,s)     |_->assert false)
  | SKIP -> (match s with V(Some(Int v))::s ->
    if v=0 then (n+1,e,s) else (n+3,e,s)     |_->assert false)
  | PAIR -> (match s with V(Some t)::V(Some t')::s ->
    (n+1,e,V(Some(Node(t',t)))::s)     |_->assert false)
  | SPLIT -> (match s with V(Some (Node(t',t)))::s ->
    (n+1,e,V(Some t)::V(Some t')::s)     |_->assert false)
  | SWAP -> (match s with x::y::s ->
    (n+1,e,y::x::s)     |_->assert false)
  | ISNULL -> (match s with V(Some t)::s ->
    (n+1,e,V(Some(Int(if t=Null then 1 else 0)))::s)     |_->assert false)
  | ISINT -> (match s with V(Some t)::s ->
    (n+1,e,V(Some(Int(match t with Int _->1|_->0)))::s)     |_->assert false)
  | SUCC -> (match s with V(Some(Int v))::s ->
    (n+1,e,V(Some(Int(v+1)))::s)     |_->assert false)
  | PRED -> (match s with V(Some(Int v))::s ->
    (n+1,e,V(Some(Int(v-1)))::s)     |_->assert false)
  | LEQ -> (match s with V(Some(Int v))::V(Some(Int w))::s ->
    (n+1,e,V(Some(Int(if w<=v then 1 else 0)))::s)     |_->assert false)
  | HALT -> raise (Halt (n,s))



let main (l : instr list) =
  let t_instr = Array.of_list l in
  let rec loop_step state = loop_step (step t_instr state) in
  let n,s = try loop_step (1,Env.empty,[]) with Halt (n,s) -> n,s in
  if n=0 then match s with
    | [V(Some t)] -> Format.printf "resultat de l'execution du binaire : "; Tree.print t; Format.printf "@."
    | [_] -> Format.eprintf "execution du binaire : le resultat n'est pas un arbre@."
    | _ -> Format.eprintf "execution du binaire : la pile n'est pas de taille 1@."
  else Format.eprintf "execution du binaire : ne termine pas sur l'instruction 0@."

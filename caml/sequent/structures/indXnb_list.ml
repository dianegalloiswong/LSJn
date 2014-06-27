type t = (int*int) list (* indice, combien de fois l'indice est présent (>0) *)
(* par ordre décroissant d'indice *)

let empty = []

let rec mem_g n = function
  | [] -> false
  | (i,_)::_ when i<=n -> true
  | _::t -> mem_g n t

let rec mem_d n = function
  | (i,_)::_ when i=n -> true
  | (i,_)::t when i>n -> mem_d n t
  | _ -> false

let rec add i = function
  | (j,nb)::t when j=i -> (i,nb+1)::t
  | (j,nb)::t when j>i -> (j,nb)::(add i t)
  | l -> (i,1)::l

let rec rm i = function
  | (j,nb)::t when j=i -> if nb=1 then t else (j,nb-1)::t
  | (j,nb)::t when j>i -> (j,nb)::(rm i t)
  | _ -> raise Not_found




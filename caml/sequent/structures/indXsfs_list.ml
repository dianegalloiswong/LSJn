type t = (int*(int list)) list
type couple = int*int

let empty = []

let rec add (i,a) = function
  | (j,l)::t when j=i -> (j,a::l)::t
  | (j,l)::t when j>i -> (j,l)::(add (i,a) t)
  | l -> (i,[a])::l

let rec enleve x = function
  | [] -> raise Not_found
  | h::t when h=x -> t
  | h::t -> h::(enleve x t)
let rec rm (i,a) = function
  | (j,l)::t when j=i ->
    let l = enleve a l in
    if l=[] then t else (j,l)::t
  | (j,l)::t when j>i -> (j,l)::(rm (i,a) t)
  | _ -> raise Not_found

(* _inf *)
let rec trouve_n n = function
  | [] -> raise Not_found
  | (i,l)::_ when i<=n -> (i,List.hd l)
  | _::t -> trouve_n n t

let rec filter_n n = function
  | [] -> []
  | (i,l)::t when i<=n ->
    let lc = filter_n n t in
    (List.map (fun a -> (i,a)) l)@lc
  | _::t -> filter_n n t

let rec fold_n n f init = function
  | [] -> init
  | (i,l)::t when i<=n -> fold_n n f (List.fold_left (fun v a -> f v (i,a)) init l) t
  | _::t -> fold_n n f init t




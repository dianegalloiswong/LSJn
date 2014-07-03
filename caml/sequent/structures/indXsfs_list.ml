type t = (int*(int list)) list
type couple = int*int

let empty = []

let rec add (i,a) = function
  | (j,l)::t when j=i -> (j,a::l)::t
  | (j,l)::t when j>i -> (j,l)::(add (i,a) t)
  | l -> (i,[a])::l

let rec mem (i,a) = function
  | (j,l)::t when j=i -> List.mem a l
  | (j,l)::t when j>i -> mem (i,a) t
  | _ -> false

let rec print = function
  | (i,l)::t -> Format.printf "( %d , [ " i; List.iter (fun a -> Format.printf "%d; " a) l;
Format.printf "] ); "; print t
  | [] -> ()

let rec enleve x = function
  | [] -> assert false
  | h::t when h=x -> t
  | h::t -> h::(enleve x t)
let rec rm (i,a) = function
  | (j,l)::t when j=i ->
    let l = enleve a l in
    if l=[] then t else (j,l)::t
  | (j,l)::t when j>i -> (j,l)::(rm (i,a) t)
  | [] -> assert false
  | (j,_)::_ as t -> Format.printf "i=%d, j=%d, a=%d, %d appels@." i j a !Time.appels;
print t; assert false

let c = (1,5)
let add2 (i,a) t = Format.printf "add (%d,%d) (%b);  " i a (mem c t);let r= add (i,a) t in
Format.printf "%b@." (mem c r);r
let rm2 (i,a) t = Format.printf "rm (%d,%d) (%b);  " i a (mem c t); let r=rm (i,a) t in
Format.printf "%b@." (mem c r);r

let rec trouve_inf n = function
  | [] -> raise Not_found
  | (i,l)::_ when i<=n -> (i,List.hd l)
  | _::t -> trouve_inf n t
let trouve_eq n = function
  | (i,l)::_ when i=n -> (i,List.hd l)
  | (i,_)::_ when i>n -> assert false
  | _ -> raise Not_found


let rec filter_inf n = function
  | [] -> []
  | (i,l)::t when i<=n ->
    let lc = filter_inf n t in
    (List.map (fun a -> (i,a)) l)@lc
  | _::t -> filter_inf n t
let filter_eq n = function
  | (i,l)::_ when i=n -> List.map (fun a -> (n,a)) l
  | (i,_)::_ when i>n -> assert false
  | _ -> []

let rec fold_inf n f init = function
  | [] -> init
  | (i,l)::t when i<=n -> fold_inf n f (List.fold_left (fun v a -> f v (i,a)) init l) t
  | _::t -> fold_inf n f init t
(*let fold_eq n f init = function
  | (i,l)::_ -> List.fold_left (fun v a -> f v (n,a) init l)
  | (i,_)::_ when i>n -> assert false
  | _ -> init*)
				   




let rec nombre_inf n = function
  | [] -> 0
  | (i,l)::t when i<=n -> (List.length l)+(nombre_inf n t)
  | _::t -> nombre_inf n t
let rec nombre_eq n = function
  | (i,l)::t when i=n -> List.length l
  | (i,_)::_ when i>n -> assert false
  | _ -> 0



(*
let rec insert_fin x = function
  | [] -> [x]
  | h::t -> h::(insert_fin x t)
let rotat l = insert_fin (List.hd l) (List.tl l)
*)
let rec concat_fin l = function
  | [] -> l
  | h::t -> h::(concat_fin l t)
let rec separe_k k = function
  | [] -> assert false
  | h::t -> if k=1 then [h],t else if k=0 then [],t
    else let (l1,l2) = separe_k (k-1) t in (h::l1,l2)
let rotat_k k l = let (l1,l2) = separe_k k l in concat_fin l1 l2
let rotat l = rotat_k 1 l

let rec nth_inf n k = function
  | [] -> assert false
  | (i,l)::t when i<=n ->
    let p = List.length l in
    if k<p then
      let l = rotat l in
      ( (i,List.hd l), (i,l)::t )
    else
      let c,t = nth_inf n (k-p) t in
      (c,(i,l)::t)
  | h::t -> let c,t = nth_inf n k t in (c,h::t)
				    
let nth_eq n k = function
  | (i,l)::t when i=n ->
    assert (k<List.length l);
    let l = rotat l in
    ( (n,List.hd l), (n,l)::t )
  | _ -> assert false


let rec reord_inf n k = function
  | [] -> assert false
  | (i,l)::t when i<=n ->
    let p = List.length l in
    if k<p then
      let l = rotat_k (p-k) l in
      (i,l)::t
    else if k=p then
      (i,l)::t
    else
      (i,l)::(reord_inf n (k-p) t)
  | h::t -> h::(reord_inf n k t)

let reord_eq n k = function
  | (i,l)::t when i=n ->
    if k = List.length l then
      (i,l)::t
    else
      let l = rotat_k k l in
      (i,l)::t
  | _ -> assert false

(*
let nombre_inf n t = Format.printf "nombre_inf, n=%d, t: " n; print t; Format.printf "@.";
  nombre_inf n t
 

let nth_inf n k t = Format.printf "  nth_inf n=%d k=%d " n k; print t;
  let c,t = nth_inf n k t in
  Format.printf "  :  (%d,%d),   " (fst c) (snd c); print t; Format.printf "@.";
  c,t
*)

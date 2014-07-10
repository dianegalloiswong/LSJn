open Def
open Global_ref
open Precalculs


let compare_prio_g n (p,c) (i,a) =
  if i<=n && !priorite.(a)<p then !priorite.(a),(i,a) else p,c
let compare_prio_d n (p,c) (i,a) =
  if i=n && !priorite.(a)<p then !priorite.(a),(i,a) else p,c

let plus_forte_priorite seq =
  let p,c = List.fold_left (compare_prio_g seq.n) (6,(0,0)) seq.g in
  let p,c = List.fold_left (compare_prio_d seq.n) (p,c) seq.d in
  p,c

(*
let rec nb_inf n = function
  | [] -> 0
  | (i,a)::t -> (if i<=n && (match !sf.(a) with C(Imp,_,_)->true|_->false) then 1 else 0) + nb_inf n t
let nb_imp_g seq = nb_inf seq.n seq.g
*)
let rec all_imp_inf n = function
  | [] -> []
  | (i,a)::t when i<=n && (match !sf.(a) with C(Imp,_,_)->true|_->false) -> (i,a)::(all_imp_inf n t)
  | _::t -> all_imp_inf n t
let all_imp_g seq = all_imp_inf seq.n seq.g

let rec all_imp_eq n = function
  | [] -> []
  | (i,a)::t when i=n && (match !sf.(a) with C(Imp,_,_)->true|_->false) -> (i,a)::(all_imp_eq n t)
  | _::t -> all_imp_eq n t
let all_imp_d seq = all_imp_eq seq.n seq.d

let nb_imp_g seq = List.length (all_imp_g seq)
let nth_imp_g k seq = List.nth (List.sort compare (all_imp_g seq)) k

let nb_imp_d seq = List.length (all_imp_d seq)
let nth_imp_d k seq = List.nth (List.sort compare (all_imp_d seq)) k




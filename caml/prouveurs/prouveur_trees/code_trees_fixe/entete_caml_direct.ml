(*entete_caml_direct*)

(*equal ?*)
(* 
compare = compare
pred n = n-1
length = List.length
mem
sort
nth
*)

(* utilities *)

let rec rm x = function
  | [] -> assert false
  | h::t when h=x -> t
  | h::t -> h::(rm x t)

let rec mem_inf (n,x) = function
  | [] -> false
  | (i,y)::_ when i<=n && y=x -> true
  | _::t -> mem_inf (n,x) t


(* sequent *)

type sequent = { mutable g : couple list; mutable n : int; mutable d : couple list; mutable fauxL : bool; mutable id : bool }

let seq = { g=[]; d=[]; n=0; cl_g=[]; cl_d=[]; fauxL=false; id=false }

(***)
(*
let n_of_seq () = seq.n
let g_of_seq () = seq.g
let d_of_seq () = seq.d
*)
let rm_ax () = seq.fauxL<-false; seq.id<-false
(*...*)


let incr_n () =
  seq.n <- seq.n + 1;
  if mem (seq.n,0) seq.cl_g then seq.fauxL <- true

let decr_n () = seq.n <- seq.n - 1

let add_cl_g (i,cl) =
  seq.cl_g <- (i,cl)::seq.cl_g;
  if i<=seq.n then
    if cl=0 then seq.fauxL <- true
    else if List.mem (seq.n,cl) seq.cl_d then seq.id <- true
let add_cl_d (i,cl) =
  seq.cl_d <- (i,cl)::seq.cl_d;
  if i=seq.n && mem_inf (seq.n,cl) seq.cl_g then seq.id <- true
let rm_cl_g c = seq.cl_g <- rm c seq.cl_g
let rm_cl_d c = seq.cl_d <- rm c seq.cl_d

let add_g ((i,a),cl) =
  seq.g <- (i,a)::seq.g;
  add_cl_g (i,cl)
let add_d ((i,a),cl) =
  seq.d <- (i,a)::seq.d;
  add_cl_d (i,cl)
let rm_g ((i,a),cl) =
  seq.g <- rm (i,a) seq.g;
  rm_cl_g (i,cl)
let rm_d ((i,a),cl) =
  seq.d <- rm (i,a) seq.d;
  rm_cl_d (i,cl)

let programme () =
  ajout_formule_initiale ();
  prouvable ()


(* plus_forte_priorite *)

let plus_forte_priorite () =
  let x = List.fold_left (fun ( (_,(pc,_)) as xc) ( (i,(p,_)) as x) ->
    if i<=seq.n && p<pc then x else xc
  ) (0,(7,0)) seq.g in
  let (i,(p,sf)) = List.fold_left (fun ( (_,(pc,_)) as xc) ( (i,(p,_)) as x) ->
    if i=seq.n && p<pc then x else xc
  ) x seq.g in
  (p,(i,sf))


(* all_imp *)

let all_imp_g () =
  List.filter (fun (i,(p,_)) -> i<=seq.n && p=5) seq.g
let all_imp_d () =
  List.filter (fun (i,(p,_)) -> i=seq.n && p=5) seq.d

let nb_imp_g () = List.length (all_imp_g ())
let nb_imp_d () = List.length (all_imp_d ())

let nth_imp_g k = List.nth (List.sort compare (all_imp_g ())) k
let nth_imp_d k = List.nth (List.sort compare (all_imp_d ())) k


(* prouvable *)

let rec prouvable () =
  if seq.fauxL || seq.id then true else
  let (p,i_sf) = plus_forte_priorite () in
  if p=1 || p=2 then inv1prem i_sf
  else if p=3 || p=4 then inv2prem i_sf
  else if p=5 then imp ()
  else 0

and inv1prem (i,sf) =
  call_prem1 (sf,i);
  let rep = prouvable () in
  call_rev1 (sf,i);
  rep

and inv2prem (i,sf) =
  call_prem1 (sf,i);
  let rep1 = prouvable () in
  call_rev1 (sf,i);
  if rep1 then
    begin
    call_prem2 (sf,i);
    let rep2 = prouvable () in
    call_rev2 (sf,i);
    rep2
    end
  else 
    false

and impL (i,sf) =
  call_prem1 (sf,i);
  let rep1 = prouvable () in
  call_rev1 (sf,i);
  if rep1 then
    begin
    call_prem2 (sf,i);
    let rep2 = prouvable () in
    call_rev2 (sf,i);
    if rep2 then
      begin
      call_prem3 (sf,i);
      let rep3 = prouvable () in
      call_rev3 (sf,i);
      if rep3 then
	(true,true)
      else
	(false,false)
      end
    else 
      (true,false)
    end
  else 
    (true,false)

and impR (i,sf) =
  call_prem1 (sf,i);
  let rep1 = prouvable () in
  call_rev1 (sf,i);
  if rep1 then
    begin
    call_prem2 (sf,i);
    let rep2 = prouvable () in
    call_rev2 (sf,i);
    if rep2 then
      (true,true)
    else
      (false,false)
    end
  else 
    (true,false)

and imp_aux_g k nb_g nb_d =
  if k=nb_g then imp_aux_d 0 nb_d else
  let i_sf = nth_imp_g k in
  let fini,rep = impL i_sf in
  if fini then rep else imp_aux_g (k+1) nb_g nb_d
and imp_aux_d k nb_d =
  if k=nb_d then false else
  let i_sf = nth_imp_d k in
  let fini,rep = impR i_sf in
  if fini then rep else imp_aux_d (k+1) nb_d

and imp () =
  let nb_g = nb_imp_g () in
  let nb_d = nb_imp_d () in





open Def




let init_priorite () =
  let m = Array.length !sf in
  let prio = Array.make m 0 in
  prio.(0) <- 0;
  (* pol : 0 si à gauche, 1 si à droite *)
  let rec remplir i pol = match !sf.(i) with
    | CFaux -> prio.(i) <- if pol = 0 then 0 else 6
    | CVar _ -> prio.(i) <- 6
    | C (conn,i1,i2) ->
      let p = match conn,pol with
	| Et,0 -> 1
	| Ou,1 -> 2
	| Ou,0 -> 3
	| Et,1 -> 4
	| Imp,_ -> 5
	| _ -> assert false
      in prio.(i) <- p;
      remplir i1 (if conn=Imp then (1-pol) else pol);
      remplir i2 pol
  in
  remplir (m-1) 1;
  priorite := prio

(*
let prio_max () =
  let g,n,d = Seq.gnd () in
  let prio_g (i,f) = if i<=n then !priorite.(f) else -1 in
  let prio_d (i,f) = if i=n then !priorite.(f) else -1 in

  let p = List.fold_left (fun p x -> max p (prio_g x)) (-1) g in
  let p = List.fold_left (fun p x -> max p (prio_d x)) p d in
  p
*)

(* dans Seq

let prio_max () =
  let aux p a = max p !priorite.(a) in
  let p = Seq.fold_g aux (-1) in
  let p = Seq.fold_d aux p in
  p

let main () =
  let p = prio_max () in
  let qf = match p with
    | -1 -> assert false
    | 0 -> QF_aucun
    | 1 -> QF_imp
    | 2 -> QF_etR
    | 3 -> QF_ouL
    | 4 -> QF_ouR
    | 5 -> QF_etL
    | 6 -> QF_fauxL
    | _ -> assert false
  in
  let h =
    let pred a = !priorite.(a) = p in
    try
      Seq.find_g pred
    with Not_found ->
      Seq.find_d pred
  in
  qf,h
*)

(*
    try
      List.find (fun (i,f) -> i<=n && (!priorite.(f)=p)) g
    with Not_found ->
      List.find (fun (i,f) -> i=n && (!priorite.(f)=p)) d
*)

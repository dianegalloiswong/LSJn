open Def

type t = { g : (couple list) array ; mutable n : int ; d : (couple list) array ;
	   mutable cl_g : (int list) array ; mutable cl_d : (int list) array ;
	   mutable fauxL : bool ; mutable id : bool ; mutable id_sf : int
	 }


let s = { g=Array.make 7 []; n=0; d=Array.make 7 [];
	  cl_g=[||]; cl_d=[||];
	  fauxL=false; id=false; id_sf=0 }

let clear () =
  for p=0 to 6 do s.g.(p) <- [] done;
  for p=0 to 6 do s.d.(p) <- [] done;
  s.n <- 0;
  let ncl = Array.length !Def.classe in 
  s.cl_g <- Array.make ncl [];
  s.cl_d <- Array.make ncl [];
  s.fauxL <- false; s.id <- false; s.id_sf <- 0
(* choix_formule *)
(* pas nécessaire ?
let fold_g fonction init = 
  let r = ref init in
  for j=0 to 6 do
    r := List.fold_left (fun x (i,a) -> if i<=s.n then fonction x a else x) !r s.g.(j)
  done;
  !r
let fold_d fonction init =
  let r = ref init in
  for j=0 to 6 do
    List.fold_left (fun x (i,a) -> if i=s.n then fonction x a else x) init s.d.(j)
  done;
  !r
let find_g pred = List.find (fun (i,a) -> i<=s.n && (pred a)) s.g
let find_d pred = List.find (fun (i,a) -> i=s.n && (pred a)) s.d
*)



(* cl *)
(*
let add_cl_g (i,a) = let cl = classe a in s.cl_g.(cl) <- i::s.cl_g.(cl)
let add_cl_d (i,a) = let cl = classe a in s.cl_d.(cl) <- i::s.cl_d.(cl)
let rm_cl_g (i,a) = let cl = classe a in s.cl_g.(cl) <- Utilities.enleve i s.cl_g.(cl)
let rm_cl_d (i,a) = let cl = classe a in s.cl_d.(cl) <- Utilities.enleve i s.cl_d.(cl)
*)
let mem_cl_g cl = List.exists (fun i -> i<=s.n) s.cl_g.(cl)
let mem_cl_d cl = List.mem s.n s.cl_d.(cl)

let add_cl_g (i,a) = 
  let cl = !classe.(a) in
  s.cl_g.(cl) <- i::s.cl_g.(cl);
  if i<=s.n then
    (
      if cl = 0 then s.fauxL <- true;
      if mem_cl_d cl then (s.id <- true; s.id_sf <- a)
    )
let add_cl_d (i,a) = 
  let cl = !classe.(a) in
  s.cl_d.(cl) <- i::s.cl_d.(cl);
  if (i=s.n) && (mem_cl_g cl) then (s.id <- true; s.id_sf <- a)

let rm_ax () = s.fauxL <- false; s.id <- false
let rm_cl_g (i,a) = 
  let cl = !classe.(a) in 
  s.cl_g.(cl) <- Utilities.enleve i s.cl_g.(cl)
let rm_cl_d (i,a) = 
  let cl = !classe.(a) in 
  s.cl_d.(cl) <- Utilities.enleve i s.cl_d.(cl)

let cl_post_incr_n () =
(*  List.iter (fun (i,a) -> if i=s.n then let cl = !classe.(a) in (if cl=0 then s.fauxL <- true; if mem_cl_d cl then (s.id <- true; s.id_sf <- a))) s.g;
  List.iter (fun (i,a) -> if (i=s.n) && (mem_cl_g !classe.(a)) then (s.id <- true; s.id_sf <- a)) s.d*)
  ()


(* structure de .g et .d *)
let add_g c = 
  let p = !priorite.(snd c) in s.g.(p) <- Lcord.add c s.g.(p);
  add_cl_g c
let add_d c = 
  let p = !priorite.(snd c) in s.d.(p) <- Lcord.add c s.d.(p);
  add_cl_d c
let rm_g c =
  let p = !priorite.(snd c) in s.g.(p) <- Lcord.rm c s.g.(p);
  rm_cl_g c
let rm_d c = 
  let p = !priorite.(snd c) in s.d.(p) <- Lcord.rm c s.d.(p);
  rm_cl_d c



let of_sous_formule a =
  clear ();
  add_d (0,a)

let of_sous_formule_CL a = 
  clear ();
  add_d (0,a);
  add_g (1,0)

let n () = s.n

let inf_n i = i<=s.n
let eq_n n = n=s.n


let incr_n () = s.n <- s.n + 1; cl_post_incr_n ()
let decr_n () = s.n <- s.n - 1; rm_ax ()



(*
let check_id () =
  List.exists (fun (i,a) -> i<=s.n &&
    List.exists (fun (i,b) -> i=s.n && !Def.classe.(a) = !Def.classe.(b)) s.d
  ) s.g
*)
let check_id () = if s.id then Some s.id_sf else None


(* imp *)

(*
let is_imp f = match !sf.(f) with C (Imp,_,_) -> true | _ -> false
let is_imp_g (i,a) = i<=s.n && is_imp a
let is_imp_d (i,a) = i=s.n && is_imp a

let all_imp_g () = List.filter is_imp_g s.g
let all_imp_d () = List.filter is_imp_d s.d
*)
let all_imp_g () = Lcord.filter_inf s.n s.g.(5)
let all_imp_d () = Lcord.filter_eq s.n s.d.(5)

let nombre_imp_g () = List.length (all_imp_g ())
let nombre_imp_d () = List.length (all_imp_d ())

let nth_imp_g k = List.nth (List.sort compare (all_imp_g ())) k
let nth_imp_d k = List.nth (List.sort compare (all_imp_d ())) k




let var_g () = List.fold_left (fun l (i,a) ->
  if i<=s.n then match !sf.(a) with
    | CVar x -> x::l
    | _ -> l
  else l
) [] s.g.(6)





let prio_g () =
  let rec aux p =
    if p = 7 then 7,(0,0)
    else
      try p,(Lcord.trouve_inf s.n s.g.(p))
      with Not_found -> aux (p+1)
  in aux 0

let prio_d () =
  let rec aux p =
    if p = 7 then 7,(0,0)
    else
      try p,(Lcord.trouve_eq s.n s.d.(p))
      with Not_found -> aux (p+1)
  in aux 0

let choix_formule () =
  let pg,hg = prio_g () and pd,hd = prio_d () in
  let p = min pg pd in
  let h = if p=pg then hg else hd in
  let qf = match p with
    | 0 -> QF_fauxL
    | 1 -> QF_etL
    | 2 -> QF_ouR
    | 3 -> QF_ouL
    | 4 -> QF_etR
    | 5 -> QF_imp
    | 6 -> QF_aucun
    | 7 -> 
Format.printf " %d " !priorite.(1);
Utilities.print_couple_list s.d.(6);
assert false
    | _ -> assert false
  in
  qf,h

(*
let prio_max () =
  let aux p a = max p !priorite.(a) in
  let p = fold_g aux (-1) in
  let p = fold_d aux p in
  

let prio_min () =
  let aux p a = min p !priorite.(a) in
  let p = fold_g aux 7 in
  let p = fold_d aux p in
  p

let choix_formule () =
  let p = prio_min () in
  let qf = match p with
    | 0 -> QF_fauxL
    | 1 -> QF_etL
    | 2 -> QF_ouR
    | 3 -> QF_ouL
    | 4 -> QF_etR
    | 5 -> QF_imp
    | 6 -> QF_aucun
    | 7 -> assert false
    | _ -> assert false
  in
  let h =
    let pred a = !priorite.(a) = p in
    try
      find_g pred
    with Not_found ->
      find_d pred
  in
  qf,h
*)

(*
    | 0 -> QF_aucun
    | 1 -> QF_imp
    | 2 -> QF_etR
    | 3 -> QF_ouL
    | 4 -> QF_ouR
    | 5 -> QF_etL
    | 6 -> QF_fauxL
*)

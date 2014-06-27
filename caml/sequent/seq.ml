open Def
open Global_ref

module G = Structs.G
module D = Structs.D

module Cl_g = Structs.Cl_g
module Cl_d = Structs.Cl_d

type t = { mutable g : G.t ; mutable n : int ; mutable d : D.t ;
	   mutable cl_g : Cl_g.t ; mutable cl_d : Cl_d.t ;
	   mutable fauxL : bool ; mutable id : bool ; mutable id_sf : int
	 }


let s = { g=G.empty(); n=0; d=D.empty();
	  cl_g=Cl_g.empty 0; cl_d=Cl_d.empty 0;
	  fauxL=false; id=false; id_sf=0 }

let clear () =
  s.g <- G.empty();
  s.d <- D.empty();
  s.n <- 0;
  let ncl = Array.length !classe in 
  s.cl_g <- Cl_g.empty ncl;
  s.cl_d <- Cl_d.empty ncl;
  s.fauxL <- false; s.id <- false; s.id_sf <- 0



(* cl *)

let mem_cl_g cl = Cl_g.mem cl s.n s.cl_g
let mem_cl_d cl = Cl_d.mem cl s.n s.cl_d

let add_cl_g (i,a) = 
  let cl = !classe.(a) in
  Cl_g.add i cl s.cl_g;
  if i<=s.n then
    (
      if cl = 0 then s.fauxL <- true;
      if mem_cl_d cl then (s.id <- true; s.id_sf <- a)
    )
let add_cl_d (i,a) = 
  let cl = !classe.(a) in
  Cl_d.add i cl s.cl_d;
  if (i=s.n) && (mem_cl_g cl) then (s.id <- true; s.id_sf <- a)

let rm_ax () = s.fauxL <- false; s.id <- false
let rm_cl_g (i,a) = 
  let cl = !classe.(a) in 
  Cl_g.rm i cl s.cl_g
let rm_cl_d (i,a) = 
  let cl = !classe.(a) in 
  Cl_d.rm i cl s.cl_d

let cl_post_incr_n () =
(*  List.iter (fun (i,a) -> if i=s.n then let cl = !classe.(a) in (if cl=0 then s.fauxL <- true; if mem_cl_d cl then (s.id <- true; s.id_sf <- a))) s.g;
  List.iter (fun (i,a) -> if (i=s.n) && (mem_cl_g !classe.(a)) then (s.id <- true; s.id_sf <- a)) s.d*)
  ()

(* fin cl *)




let add_g c = 
  G.add c s.g;
  add_cl_g c
let add_d c = 
  D.add c s.d;
  add_cl_d c
let rm_g c =
  G.rm c s.g;
  rm_cl_g c
let rm_d c = 
  D.rm c s.d;
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



let check_id () = if s.id then Some s.id_sf else None



(* imp *)

(*
let all_imp_g () = G.filter_n s.n s.g.(5)
let all_imp_d () = D.filter_n s.n s.d.(5)
*)
let all_imp_g () = G.all_imp s.n s.g
let all_imp_d () = D.all_imp s.n s.d

let nombre_imp_g () = List.length (all_imp_g ())
let nombre_imp_d () = List.length (all_imp_d ())

let nth_imp_g k = List.nth (List.sort compare (all_imp_g ())) k
let nth_imp_d k = List.nth (List.sort compare (all_imp_d ())) k






let var_g () =
  if !Affichage.cmods then
    G.fold_vars s.n (fun l (i,a) -> match !sf.(a) with
      | CVar x -> x::l
      | _ -> l
    ) [] s.g
  else []


let prio_g () =
  G.priorite_plus_forte s.n s.g

let prio_d () =
  D.priorite_plus_forte s.n s.d

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
    | 7 -> assert false
    | _ -> assert false
  in
  qf,h


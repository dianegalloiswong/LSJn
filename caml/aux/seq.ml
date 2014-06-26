open Def


module type G =
sig
  type t
  val empty : t
  val add : couple -> t -> t
  val rm : couple -> t -> t
  val filter_inf : int -> t -> (couple list)
  val trouve_inf : int -> t -> couple
  val fold_inf : int -> ('a -> couple -> 'a) -> 'a -> t -> 'a
end;;
module type D =
sig
  type t
  val empty : t
  val add : couple -> t -> t
  val rm : couple -> t -> t
  val filter_eq : int -> t -> (couple list)
  val trouve_eq : int -> t -> couple
end;;


module G = (Lcord : G)
module D = (Lcord : D)


type t = { g : G.t array ; mutable n : int ; d : D.t array ;
	   mutable cl_g : (int list) array ; mutable cl_d : (int list) array ;
	   mutable fauxL : bool ; mutable id : bool ; mutable id_sf : int
	 }


let s = { g=Array.make 7 G.empty; n=0; d=Array.make 7 D.empty;
	  cl_g=[||]; cl_d=[||];
	  fauxL=false; id=false; id_sf=0 }

let clear () =
  for p=0 to 6 do s.g.(p) <- G.empty done;
  for p=0 to 6 do s.d.(p) <- D.empty done;
  s.n <- 0;
  let ncl = Array.length !Def.classe in 
  s.cl_g <- Array.make ncl [];
  s.cl_d <- Array.make ncl [];
  s.fauxL <- false; s.id <- false; s.id_sf <- 0





(* cl *)

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
  let p = !priorite.(snd c) in s.g.(p) <- G.add c s.g.(p);
  add_cl_g c
let add_d c = 
  let p = !priorite.(snd c) in s.d.(p) <- D.add c s.d.(p);
  add_cl_d c
let rm_g c =
  let p = !priorite.(snd c) in s.g.(p) <- G.rm c s.g.(p);
  rm_cl_g c
let rm_d c = 
  let p = !priorite.(snd c) in s.d.(p) <- D.rm c s.d.(p);
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
let is_imp f = match !sf.(f) with C (Imp,_,_) -> true | _ -> false
let is_imp_g (i,a) = i<=s.n && is_imp a
let is_imp_d (i,a) = i=s.n && is_imp a

let all_imp_g () = List.filter is_imp_g s.g
let all_imp_d () = List.filter is_imp_d s.d
*)
let all_imp_g () = G.filter_inf s.n s.g.(5)
let all_imp_d () = D.filter_eq s.n s.d.(5)

let nombre_imp_g () = List.length (all_imp_g ())
let nombre_imp_d () = List.length (all_imp_d ())

let nth_imp_g k = List.nth (List.sort compare (all_imp_g ())) k
let nth_imp_d k = List.nth (List.sort compare (all_imp_d ())) k




let var_g () = 
  if !details || !aff_cmods then
    G.fold_inf s.n (fun l (i,a) -> match !sf.(a) with
      | CVar x -> x::l
      | _ -> l
    ) [] s.g.(6)
  else []


let prio_g () =
  let rec aux p =
    if p = 7 then 7,(0,0)
    else
      try p,(G.trouve_inf s.n s.g.(p))
      with Not_found -> aux (p+1)
  in aux 0

let prio_d () =
  let rec aux p =
    if p = 7 then 7,(0,0)
    else
      try p,(D.trouve_eq s.n s.d.(p))
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
    | 7 -> assert false
    | _ -> assert false
  in
  qf,h


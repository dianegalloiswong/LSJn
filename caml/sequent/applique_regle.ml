open Def
open Global_ref



  let etL_prem (i,h) = assert (Seq.inf_n i);
(*if not (Seq.eq_n i) then Format.printf " i:%d, n:%d, h:%s " i (Seq.n()) (To_string.sous_formule h); *)
    assert (Seq.eq_n i); (* pas nécesssaire pour que l'algo soit juste mais probablement vrai -> test *)
    match !sf.(h) with C(Et,a,b) ->
    let n = Seq.n () in
    Seq.rm_g (i,h);
    Seq.add_g (n,a);
    Seq.add_g (n,b)
    | _ -> assert false

  let etL_rev (i,h) = assert (Seq.inf_n i); match !sf.(h) with C(Et,a,b) ->
    let n = Seq.n () in
    Seq.rm_g (n,a);
    Seq.rm_g (n,b);
    Seq.add_g (i,h);
    Seq.rm_ax ()
    | _ -> assert false

  let ouR_prem (n,h) = assert (Seq.eq_n n); match !sf.(h) with C(Ou,a,b) ->
    Seq.rm_d (n,h);
    Seq.add_d (n,a);
    Seq.add_d (n,b)
    | _ -> assert false

  let ouR_rev (n,h) = assert (Seq.eq_n n); match !sf.(h) with C(Ou,a,b) ->
    Seq.rm_d (n,a);
    Seq.rm_d (n,b);
    Seq.add_d (n,h);
    Seq.rm_ax ()
    | _ -> assert false

  let ouL1_prem (i,h) = assert (Seq.inf_n i);assert (Seq.eq_n i); match !sf.(h) with C(Ou,a,_) ->
    let n = Seq.n () in
    Seq.rm_g (i,h);
    Seq.add_g (n,a)
    | _ -> assert false
 
  let ouL1_rev (i,h) = assert (Seq.inf_n i); match !sf.(h) with C(Ou,a,_) ->
    let n = Seq.n () in
    Seq.rm_g (n,a);
    Seq.add_g (i,h);
    Seq.rm_ax ()
    | _ -> assert false

  let ouL2_prem (i,h) = assert (Seq.inf_n i);assert (Seq.eq_n i); match !sf.(h) with C(Ou,_,b) ->
    let n = Seq.n () in
    Seq.rm_g (i,h);
    Seq.add_g (n,b)
    | _ -> assert false
 
  let ouL2_rev (i,h) = assert (Seq.inf_n i); match !sf.(h) with C(Ou,_,b) ->
    let n = Seq.n () in
    Seq.rm_g (n,b);
    Seq.add_g (i,h);
    Seq.rm_ax ()
    | _ -> assert false

  let etR1_prem (n,h) = assert (Seq.eq_n n); match !sf.(h) with C(Et,a,_) ->
    Seq.rm_d (n,h);
    Seq.add_d (n,a)
    | _ -> assert false

  let etR1_rev (n,h) = assert (Seq.eq_n n); match !sf.(h) with C(Et,a,_) ->
    Seq.rm_d (n,a);
    Seq.add_d (n,h);
    Seq.rm_ax ()
    | _ -> assert false
 
  let etR2_prem (n,h) = assert (Seq.eq_n n); match !sf.(h) with C(Et,_,b) ->
    Seq.rm_d (n,h);
    Seq.add_d (n,b)
    | _ -> assert false
  
  let etR2_rev (n,h) = assert (Seq.eq_n n); match !sf.(h) with C(Et,_,b) ->
    Seq.rm_d (n,b);
    Seq.add_d (n,h);
    Seq.rm_ax ()
    | _ -> assert false


  let impL1_prem (i,h) = assert (Seq.inf_n i); match !sf.(h) with C(Imp,_,b) ->
    let n = Seq.n () in
    Seq.rm_g (i,h);
    Seq.add_g (n,b)
    | _ -> assert false
  
  let impL1_rev (i,h) = assert (Seq.inf_n i); match !sf.(h) with C(Imp,_,b) ->
    let n = Seq.n () in
    Seq.rm_g (n,b);
    Seq.add_g (i,h);
    Seq.rm_ax ()
    | _ -> assert false

  let impL2_prem (i,h) = assert (Seq.inf_n i); match !sf.(h) with C(Imp,a,b) ->
    let n = Seq.n () in
    Seq.rm_g (i,h);
    Seq.add_g (n+1,b);
    Seq.add_d (n,a)
    | _ -> assert false
 
  let impL2_rev (i,h) = assert (Seq.inf_n i); match !sf.(h) with C(Imp,a,b) ->
    let n = Seq.n () in
    Seq.rm_g (n+1,b);
    Seq.add_g (i,h);
    Seq.rm_d (n,a);
    Seq.rm_ax ()
    | _ -> assert false
  
  let impL3_prem (i,h) = assert (Seq.inf_n i); match !sf.(h) with C(Imp,a,b) ->
    let n = Seq.n () in
    Seq.incr_n ();
    Seq.rm_g (i,h);
    Seq.add_g (n+2,b);
    Seq.add_d (n+1,a)
    | _ -> assert false
  
  let impL3_rev (i,h) = assert (Seq.inf_n (i+1)); match !sf.(h) with C(Imp,a,b) ->
    let n = (Seq.n ())-1 in
    Seq.rm_g (n+2,b);
    Seq.add_g (i,h);
    Seq.rm_d (n+1,a);
    Seq.decr_n ();
    Seq.rm_ax ()
    | _ -> assert false


  let impR1_prem (n,h) = assert (Seq.eq_n n); match !sf.(h) with C(Imp,a,b) ->
    Seq.rm_d (n,h);
    Seq.add_g (n,a);
    Seq.add_d (n,b)
    | _ -> assert false

  let impR1_rev (n,h) = assert (Seq.eq_n n); match !sf.(h) with C(Imp,a,b) ->
    Seq.rm_g (n,a);
    Seq.rm_d (n,b);
    Seq.add_d (n,h);
    Seq.rm_ax ()
    | _ -> assert false

  let impR2_prem (n,h) = assert (Seq.eq_n n); match !sf.(h) with C(Imp,a,b) ->
    Seq.incr_n ();
    Seq.rm_d (n,h);
    Seq.add_g (n+1,a);
    Seq.add_d (n+1,b)
    | _ -> assert false

  let impR2_rev (n,h) = assert (Seq.eq_n (n+1)); match !sf.(h) with C(Imp,a,b) ->
    Seq.rm_g (n+1,a);
    Seq.rm_d (n+1,b);
    Seq.add_d (n,h);
    Seq.decr_n ();
    Seq.rm_ax ()
    | _ -> assert false
 


(* ----------- *)










(*
  let etL_prem (i,h) = assert (Seq.inf_n i); match sf.(h) with C(Et,a,b) ->
    Seq.rm_g (i,h);
    Seq.add_g (i,a);
    Seq.add_g (i,b)
    | _ -> assert false
  in
  let etL_rev (i,h) = assert (Seq.inf_n i); match sf.(h) with C(Et,a,b) ->
    Seq.rm_g (i,a);
    Seq.rm_g (i,b);
    Seq.add_g (i,h)
    | _ -> assert false
  in
  let ouR_prem (n,h) = assert (Seq.eq_n n); match sf.(h) with C(Ou,a,b) ->
    Seq.rm_d (n,h);
    Seq.add_d (n,a);
    Seq.add_d (n,b)
    | _ -> assert false
  in
  let ouR_rev (n,h) = assert (Seq.eq_n n); match sf.(h) with C(Ou,a,b) ->
    Seq.rm_d (n,a);
    Seq.rm_d (n,b);
    Seq.add_d (n,h)
    | _ -> assert false
  in
  let ouL1_prem (i,h) = assert (Seq.inf_n i); match sf.(h) with C(Ou,a,_) ->
    Seq.rm_g (i,h);
    Seq.add_g (i,a)
    | _ -> assert false
  in
  let ouL1_rev (i,h) = assert (Seq.inf_n i); match sf.(h) with C(Ou,a,_) ->
    Seq.rm_g (i,a);
    Seq.add_g (i,h)
    | _ -> assert false
  in
  let ouL2_prem (i,h) = assert (Seq.inf_n i); match sf.(h) with C(Ou,_,b) ->
    Seq.rm_g (i,h);
    Seq.add_g (i,b)
    | _ -> assert false
  in
  let ouL2_rev (i,h) = assert (Seq.inf_n i); match sf.(h) with C(Ou,_,b) ->
    Seq.rm_g (i,b);
    Seq.add_g (i,h)
    | _ -> assert false
  in
  let etR1_prem (n,h) = assert (Seq.eq_n n); match sf.(h) with C(Et,a,_) ->
    Seq.rm_d (n,h);
    Seq.add_d (n,a)
    | _ -> assert false
  in
  let etR1_rev (n,h) = assert (Seq.eq_n n); match sf.(h) with C(Et,a,_) ->
    Seq.rm_d (n,a);
    Seq.add_d (n,h)
    | _ -> assert false
  in
  let etR2_prem (n,h) = assert (Seq.eq_n n); match sf.(h) with C(Et,_,b) ->
    Seq.rm_d (n,h);
    Seq.add_d (n,b)
    | _ -> assert false
  in
  let etR2_rev (n,h) = assert (Seq.eq_n n); match sf.(h) with C(Et,_,b) ->
    Seq.rm_d (n,b);
    Seq.add_d (n,h)
    | _ -> assert false
  in

  let impL1_prem (i,h) = assert (Seq.inf_n i); match sf.(h) with C(Imp,_,b) ->
    Seq.rm_g (i,h);
    Seq.add_g (i,b)
    | _ -> assert false
  in
  let impL1_rev (i,h) = assert (Seq.inf_n i); match sf.(h) with C(Imp,_,b) ->
    Seq.rm_g (i,b);
    Seq.add_g (i,h)
    | _ -> assert false
  in
  let impL2_prem (i,h) = assert (Seq.inf_n i); match sf.(h) with C(Imp,a,b) ->
    let n = Seq.n () in
    Seq.rm_g (i,h);
    Seq.add_g (n+1,b);
    Seq.add_d (n,a)
    | _ -> assert false
  in
  let impL2_rev (i,h) = assert (Seq.inf_n i); match sf.(h) with C(Imp,a,b) ->
    let n = Seq.n () in
    Seq.rm_g (n+1,b);
    Seq.add_g (i,h);
    Seq.rm_d (n,a)
    | _ -> assert false
  in
  let impL3_prem (i,h) = assert (Seq.inf_n i); match sf.(h) with C(Imp,a,b) ->
    let n = Seq.n () in
    Seq.rm_g (i,h);
    Seq.add_g (n+2,b);
    Seq.add_d (n+1,a);
    Seq.incr_n ()
    | _ -> assert false
  in
  let impL3_rev (i,h) = assert (Seq.inf_n (i+1)); match sf.(h) with C(Imp,a,b) ->
   let n = (Seq.n ())-1 in
    Seq.rm_g (n+2,b);
    Seq.add_g (i,h);
    Seq.rm_d (n+1,a);
    Seq.decr_n ()
    | _ -> assert false
  in

  let impR1_prem (n,h) = assert (Seq.eq_n n); match sf.(h) with C(Imp,a,b) ->
    Seq.rm_d (n,h);
    Seq.add_g (0,a);
    Seq.add_d (n,b)
    | _ -> assert false
  in
  let impR1_rev (n,h) = assert (Seq.eq_n n); match sf.(h) with C(Imp,a,b) ->
    Seq.rm_g (0,a);
    Seq.rm_d (n,b);
    Seq.add_d (n,h)
    | _ -> assert false
  in
  let impR2_prem (n,h) = assert (Seq.eq_n n); match sf.(h) with C(Imp,a,b) ->
    Seq.rm_d (n,h);
    Seq.add_g (0,a);
    Seq.add_d (n+1,b);
    Seq.incr_n ()
    | _ -> assert false
  in
  let impR2_rev (n,h) = assert (Seq.eq_n (n+1)); match sf.(h) with C(Imp,a,b) ->
    Seq.rm_g (0,a);
    Seq.rm_d (n+1,b);
    Seq.add_d (n,h);
    Seq.decr_n ()
    | _ -> assert false
  in
*)




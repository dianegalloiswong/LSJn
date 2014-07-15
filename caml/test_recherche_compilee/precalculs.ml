open Def
open Global_ref


type couple = int*int

type sequent = { mutable g : couple list; mutable n : int; mutable d : couple list; mutable fauxL : bool; mutable id : bool }


type cote = L | R
let autre = function L -> R | R -> L
let cote = ref [||]


let remplir_cote () =
  let m = Array.length !sf -1 in
  let tab = Array.make (m+1) L in
  let rec aux i c = 
    tab.(i) <- c;
    match !sf.(i) with
      | CFaux | CVar _ -> ()
      | C (conn,i1,i2) ->
	aux i1 (if conn=Imp then autre c else c);
	aux i2 c
  in aux m R;
  cote := tab




let add x l = x::l
let rec rm x = function
  | [] -> raise Not_found
  | h::t when h=x -> t
  | h::t -> h::(rm x t)

let rec mem_inf n cl = function
  | [] -> false
  | (i,a)::_ when i<=n && !classe.(a)=cl -> true
  | _::t -> mem_inf n cl t

let mem_cl_g cl seq = mem_inf seq.n cl seq.g

let rec mem_eq n cl = function
  | [] -> false
  | (i,a)::_ when i=n && !classe.(a)=cl -> true
  | _::t -> mem_eq n cl t



let etL h a b =
  let prem i s =
    let n = s.n in
    let g = rm (i,h) s.g in
    let g = add (n,a) g in
    let g = add (n,b) g in
    let fauxL = !classe.(a)=0 || !classe.(b)=0 in
    let id = (mem_eq n !classe.(a) s.d) || (mem_eq n !classe.(b) s.d) in
    s.g<-g; s.fauxL<-fauxL; s.id<-id
  in
  let rev i s =
    let n = s.n in
    let g = rm (n,b) s.g in
    let g = rm (n,a) g in
    let g = add (i,h) g in
    s.g<-g; s.fauxL<-false; s.id<-false
  in
  [prem;rev]


let ouR h a b =
  let prem n s =
    assert (n = s.n);
    let d = rm (n,h) s.d in
    let d = add (n,a) d in
    let d = add (n,b) d in
    let id = (mem_inf n !classe.(a) s.g) || (mem_inf n !classe.(b) s.g) in
    s.d<-d; s.fauxL<-false; s.id<-id
  in
  let rev n s =
    let d = rm (n,b) s.d in
    let d = rm (n,a) d in
    let d = add (n,h) d in
    s.d<-d; s.fauxL<-false; s.id<-false
  in
  [prem;rev]


let ouL h a b =
  let prem1 i s =
    let n = s.n in
    let g = rm (i,h) s.g in
    let g = add (n,a) g in
    let fauxL = !classe.(a)=0 in
    let id = mem_eq n !classe.(a) s.d in
    s.g<-g; s.fauxL<-fauxL; s.id<-id
  in
  let rev1 i s =
    let n = s.n in
    let g = rm (n,a) s.g in
    let g = add (i,h) g in
    s.g<-g; s.fauxL<-false; s.id<-false
  in
  let prem2 i s =
    let n = s.n in
    let g = rm (i,h) s.g in
    let g = add (n,b) g in
    let fauxL = !classe.(b)=0 in
    let id = mem_eq n !classe.(b) s.d in
    s.g<-g; s.fauxL<-fauxL; s.id<-id
  in
  let rev2 i s =
    let n = s.n in
    let g = rm (n,b) s.g in
    let g = add (i,h) g in
    s.g<-g; s.fauxL<-false; s.id<-false
  in
  [prem1;rev1;prem2;rev2]


let etR h a b =
  let prem1 n s =
    assert (n = s.n);
    let d = rm (n,h) s.d in
    let d = add (n,a) d in
    let id = mem_inf n !classe.(a) s.g in
    s.d<-d; s.fauxL<-false; s.id<-id
  in
  let rev1 n s =
    let d = rm (n,a) s.d in
    let d = add (n,h) d in
    s.d<-d; s.fauxL<-false; s.id<-false
  in
  let prem2 n s =
    assert (n = s.n);
    let d = rm (n,h) s.d in
    let d = add (n,b) d in
    let id = mem_inf n !classe.(b) s.g in
    s.d<-d; s.fauxL<-false; s.id<-id
  in
  let rev2 n s =
    let d = rm (n,b) s.d in
    let d = add (n,h) d in
    s.d<-d; s.fauxL<-false; s.id<-false
  in
  [prem1;rev1;prem2;rev2]



let impL h a b =
  let prem1 i s =
    let n = s.n in
    let g = rm (i,h) s.g in
    let g = add (n,b) g in
    let fauxL = !classe.(b)=0 in
    let id = mem_eq n !classe.(b) s.d in
    s.g<-g; s.fauxL<-fauxL; s.id<-id
  in
  let rev1 i s =
    let n = s.n in
    let g = rm (n,b) s.g in
    let g = add (i,h) g in
    s.g<-g; s.fauxL<-false; s.id<-false
  in
  let prem2 i s =
    let n = s.n in
    let g = rm (i,h) s.g in
    let g = add (n+1,b) g in
    let d = add (n,a) s.d in
    let id = mem_inf n !classe.(a) g in
    s.g<-g; s.d<-d; s.fauxL<-false; s.id<-id
  in
  let rev2 i s =
    let n = s.n in
    let d = rm (n,a) s.d in
    let g = rm (n+1,b) s.g in
    let g = add (i,h) g in
    s.g<-g; s.d<-d; s.fauxL<-false; s.id<-false
  in
  let prem3 i s =
    let n = s.n in
    let g = rm (i,h) s.g in
    let g = add (n+2,b) g in
    let d = add (n+1,a) s.d in
    let id = mem_inf (n+1) !classe.(a) g in
    let fauxL = mem_inf (n+1) 0 g in
    s.n<-n+1; s.g<-g; s.d<-d; s.fauxL<-fauxL; s.id<-id
  in
  let rev3 i s =
    let n = s.n-1 in
    let d = rm (n+1,a) s.d in
    let g = rm (n+2,b) s.g in
    let g = add (i,h) g in
    s.n<-n; s.g<-g; s.d<-d; s.fauxL<-false; s.id<-false
  in
  [prem1;rev1;prem2;rev2;prem3;rev3]


let impR h a b =
  let prem1 n s =
    assert (n = s.n);
    let d = rm (n,h) s.d in
    let g = add (n,a) s.g in
    let d = add (n,b) d in
    let fauxL = !classe.(a)=0 in
    let id = (mem_eq n !classe.(a) d)||(mem_inf n !classe.(b) g) in
    s.g<-g; s.d<-d; s.fauxL<-fauxL; s.id<-id
  in
  let rev1 n s =
    let d = rm (n,b) s.d in
    let g = rm (n,a) s.g in
    let d = add (n,h) d in
    s.g<-g; s.d<-d; s.fauxL<-false; s.id<-false
  in
  let prem2 n s =
    assert (n = s.n);
    let d = rm (n,h) s.d in
    let g = add (n+1,a) s.g in
    let d = add (n+1,b) d in
    let id = (mem_eq (n+1) !classe.(a) d)||(mem_inf (n+1) !classe.(b) g) in
    let fauxL = mem_inf (n+1) 0 g in
    s.n<-n+1; s.g<-g; s.d<-d; s.fauxL<-fauxL; s.id<-id
  in
  let rev2 n s =
    assert (n+1 = s.n);
    let d = rm (n+1,b) s.d in
    let g = rm (n+1,a) s.g in
    let d = add (n,h) d in
    s.n<-n; s.g<-g; s.d<-d; s.fauxL<-false; s.id<-false
  in
  [prem1;rev1;prem2;rev2]







let fonctions = ref [||]

let remplir_fonctions () =
  let m = Array.length !sf -1 in
  let tab = Array.make (m+1) [] in
  for h=1 to m do
    match !sf.(h) with
      | CFaux | CVar _ -> ()
      | C (conn,a,b) ->
	let f = match conn,!cote.(h) with
	  | Et,L -> etL
	  | Et,R -> etR
	  | Ou,L -> ouL
	  | Ou,R -> ouR
	  | Imp,L -> impL
	  | Imp,R -> impR
	in tab.(h) <- f h a b
  done;
  fonctions := tab


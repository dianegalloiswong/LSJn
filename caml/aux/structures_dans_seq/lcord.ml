type t = (int*int) list

let empty = []

let rec add c = function
  | [] -> [c]
  | h::t when (fst c)<(fst h) -> h::(add c t)
  | l -> c::l


let rec rm c = function
  | [] -> raise Not_found
  | h::_ when (fst c)>(fst h) -> raise Not_found
  | h::t when h=c -> t
  | h::t -> h::(rm c t)


let rec trouve_inf n = function
  | [] -> raise Not_found
  | h::t when (fst h)<=n -> h
  | _::t -> trouve_inf n t
 
let rec trouve_eq n = function
  | [] -> raise Not_found
  | h::t when (fst h)=n -> h
  | h::t when (fst h)>n -> trouve_eq n t
  | _ -> raise Not_found

let rec filter_inf n = function
  | [] -> []
  | h::t when (fst h)<=n -> h::t
  | _::t -> filter_inf n t
 
let rec filter_eq n = function
  | [] -> []
  | h::t when (fst h)<n -> []
  | h::t when (fst h)=n -> h::(filter_eq n t)
  | _::t -> filter_eq n t


let rec fold_inf n f init = function
  | [] -> init
  | h::t when (fst h)<=n -> fold_inf n f (f init h) t
  | _::t -> fold_inf n f init t
(*
let rec fold_eq n f init = function
  | [] -> init
  | h::t when (fst h)<n -> init
  | h::t when (fst h)=n -> fold_eq n f (f init h) t
  | _::t -> fold_eq n f init t
*)

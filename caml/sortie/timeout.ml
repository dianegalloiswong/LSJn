exception Timeout

let sigalrm_handler = Sys.Signal_handle (fun _ -> raise Timeout)
(*val sigalrm_handler : Sys.signal_behavior = Sys.Signal_handle <fun>*)

let timeout time f x =
  let old_behavior = Sys.signal Sys.sigalrm sigalrm_handler in
  let reset_sigalrm () = Sys.set_signal Sys.sigalrm old_behavior 
  in ignore (Unix.alarm time) ;
  try  let res = f x in reset_sigalrm () ; res  
  with exc -> reset_sigalrm () ; raise exc



(*
let timeout f arg time default_value =
  let old_behavior = Sys.signal Sys.sigalrm sigalrm_handler in
  let reset_sigalrm () = Sys.set_signal Sys.sigalrm old_behavior 
  in ignore (Unix.alarm time) ;
  try  let res = f arg in reset_sigalrm () ; res  
  with exc -> reset_sigalrm () ;
    if exc=Timeout then default_value else raise exc
val timeout : ('a -> 'b) -> 'a -> int -> 'b -> 'b = <fun>
*)



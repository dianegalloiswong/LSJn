type t = M of (string list)*(t list)

let print a =
  let print_noeud vars chemin =
    Format.printf "  (C-M)  [ ";
    List.iter (fun n -> Format.printf "%d " n) (List.rev chemin);
    Format.printf "] : ";
    List.iter (fun s -> Format.printf "%s " s) vars;
    Format.printf "@."
  in
  let rec aux chemin = function M (vars,fils) -> 
    print_noeud vars chemin;
    aux_liste chemin 0 fils
  and aux_liste chemin i = function
    | [] -> ()
    | h::t -> aux (i::chemin) h; aux_liste chemin (i+1) t
  in aux [] a


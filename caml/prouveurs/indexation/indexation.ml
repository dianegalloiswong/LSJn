let main formule =
  Init_sf_classe.main formule;
  Init_cote.main ();
  Init_priorite.main ()


let aligne n = if n<10 then " " else ""

let cote_to_string = function Global_ref.L -> "L" | Global_ref.R -> "R"

let espaces k = String.make k ' '

let print () =
  Format.printf "  sf   classe   description   côté@.";
  for i=1 to (Array.length !Global_ref.sf)-1 do
    Format.printf "  %s%d     %s%d      %s           %s@." (aligne i) i (aligne !Global_ref.classe.(i)) !Global_ref.classe.(i) (To_string.case_sf !Global_ref.sf.(i)) (cote_to_string !Global_ref.cote.(i))
  done

let print () =
  Format.printf "  sf   classe   description   côté@.";
  for i=1 to (Array.length !Global_ref.sf)-1 do
    let sf = string_of_int i in
    let classe = string_of_int !Global_ref.classe.(i) in
    let descr = To_string.case_sf !Global_ref.sf.(i) in
    let cote = cote_to_string !Global_ref.cote.(i) in
    Format.printf "%s%s%s%s%s%s%s%s@."
      (espaces (4 - (String.length sf)))
      sf
      (espaces (7 - (String.length classe)))
      classe
      (espaces (15 - (String.length descr)))
      descr
      (espaces 6)
      cote
  done


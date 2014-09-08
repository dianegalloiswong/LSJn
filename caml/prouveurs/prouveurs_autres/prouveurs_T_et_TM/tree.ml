type tree = Null | Int of int | Node of tree*tree

let rec print = function
  | Null -> Format.printf "null"
  | Int n -> Format.printf "%d" n
  | Node (t1,t2) ->
    Format.printf "< ";
    print t1;
    Format.printf " , ";
    print t2;
    Format.printf " >"

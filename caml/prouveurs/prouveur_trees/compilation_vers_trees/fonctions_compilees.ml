let fonctions = ref ([] : Ast_trees.decl_func list)
let prefixe h = if h>=0 then "sf"^(string_of_int h)^"_" else "call_"
let prem j h = (prefixe h)^"prem"^(string_of_int j)
let rev j h = (prefixe h)^"rev"^(string_of_int j)
let nom k = match k with
  0->prem 1 |1->rev 1 |2->prem 2 |3->rev 2 |4->prem 3 |5->rev 3 |_->assert false

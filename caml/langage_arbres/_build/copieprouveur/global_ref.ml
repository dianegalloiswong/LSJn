type case_sf = CFaux | CVar of Def.variable | C of (Def.connecteur*int*int)
let sf = ref ([||] : case_sf array)
let classe = ref ([||] : int array)
let priorite = ref ([||] : int array)

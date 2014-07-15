
%{

  open Def
  open Syntaxe_fichier

%}


%token <string> IDENT
%token FOF CONJ AX
%token NON OU ET IMP EQ FAUX VRAI
%token LPAREN RPAREN
%token POINT VIR
%token EOF
%token Th ITh INTh

%right EQ
%right IMP
%left OU
%left ET
%right NON
/*%left LPAREN POINT VIR*/



%start fichier

%type <Syntaxe_fichier.fichier> fichier


%%



fichier:
  | attendus=attendu* facts=fact* EOF  { (attendus,facts) }

fact:
  | FOF LPAREN s=IDENT VIR CONJ VIR f=formule RPAREN POINT  { Conj(s,f) }
  | FOF LPAREN s=IDENT VIR AX VIR f=formule RPAREN POINT  { Ax(s,f) }
  | FOF LPAREN s=IDENT VIR s2=IDENT VIR f=formule RPAREN POINT  { Autre(s,s2,f) }

formule:
  | s=IDENT  { FVar s }
  | NON f=formule  { F (Imp,f,FFaux) }
  | f1=formule ET f2=formule  { F (Et,f1,f2) }
  | f1=formule OU f2=formule  { F (Ou,f1,f2) }
  | f1=formule IMP f2=formule  { F (Imp,f1,f2) }
  | f1=formule EQ f2=formule  { F (Et, F(Imp,f1,f2), F(Imp,f2,f1)) }
  | LPAREN f=formule RPAREN  { f }
  | FAUX  { FFaux }
  | VRAI  { F (Imp,FFaux,FFaux) }

attendu:
  | Th  { CL true }
  | ITh  { IL true } 
  | INTh  { IL false }

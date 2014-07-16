
%{

  open Ast

%}

%token NULL
%token <int> INT
%token <string> IDENT
%token MATCH WITH ARROW LET EQ IN CALL ISNULL ISINT ISNODE IF THEN ELSE
%token LT GT
%token AND LEQ //GEQ
%token PLUSUN
%token LPAREN RPAREN
%token COMMA
%token EOF

%nonassoc ARROW IN ELSE
%left AND
%left LT /*GT*/ LEQ //GEQ
%nonassoc PLUSUN
%nonassoc call
%nonassoc ISNULL ISINT ISNODE








%start prog

%type <Ast.prog> prog


%%



prog:
  | decls=decl_func* IN e=expr EOF  { (decls,e) }

decl_func:
  | LET nom=IDENT arg=IDENT EQ body=expr  { (nom,arg,body) }

expr:
  | LPAREN e=expr RPAREN  { e }
  | x=IDENT  { EVar x }
  | NULL  { ENull }
  | n=INT  { EInt n }
  | LT e1=expr COMMA e2=expr GT  { ENode (e1,e2) }
  | MATCH e1=expr WITH LT x=IDENT COMMA y=IDENT GT ARROW e2=expr  { EMatch (e1,x,y,e2) }
  | LET x=IDENT EQ e1=expr IN e2=expr  { ELetin (x,e1,e2) } 
  | CALL f=IDENT e=expr  { ECall (f,e) }               %prec call
  | ISNULL e=expr  { EIsnull e }
  | ISINT e=expr  { EIsint e }
  | ISNODE e=expr  { EIsnode e }
  | e1=expr LEQ e2=expr  { ELeq (e1,e2) }
  | IF b=expr THEN e1=expr ELSE e2=expr  { EIf (b,e1,e2) }

  | e1=expr EQ e2=expr  { EEq (e1,e2) }
  | e1=expr LT e2=expr  { ELess (e1,e2) }
  | e=expr PLUSUN  { ESucc e }
  | e1=expr AND e2=expr  { EAnd (e1,e2) }



%{

  open Ast_pos

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
%left EQ LT /*GT*/ LEQ //GEQ
%nonassoc PLUSUN
%nonassoc call
%nonassoc ISNULL ISINT ISNODE








%start prog

%type <Ast_pos.prog> prog


%%



prog:
  | decls=decl_func* IN e=expr EOF  { (decls,e) }

decl_func:
  | LET nom=ident arg=ident EQ body=expr  { (nom,arg,body) }

expr:
  | LPAREN e=expr RPAREN  { e }
  | x=ident  {( EVar x ,($startpos,$endpos))}
  | NULL  {( ENull ,($startpos,$endpos))}
  | n=INT  {( EInt n ,($startpos,$endpos))}
  | LT e1=expr COMMA e2=expr GT  {( ENode (e1,e2) ,($startpos,$endpos))}
  | MATCH e1=expr WITH LT x=ident COMMA y=ident GT ARROW e2=expr  {( EMatch (e1,x,y,e2) ,($startpos,$endpos))}
  | LET x=ident EQ e1=expr IN e2=expr  {( ELetin (x,e1,e2) ,($startpos,$endpos))} 
  | CALL f=ident e=expr  {( ECall (f,e) ,($startpos,$endpos))}               %prec call
  | ISNULL e=expr  {( EIsnull e ,($startpos,$endpos))}
  | ISINT e=expr  {( EIsint e ,($startpos,$endpos))}
  | ISNODE e=expr  {( EIsnode e ,($startpos,$endpos))}
  | e1=expr LEQ e2=expr  {( ELeq (e1,e2) ,($startpos,$endpos))}
  | IF b=expr THEN e1=expr ELSE e2=expr  {( EIf (b,e1,e2) ,($startpos,$endpos))}

  | e1=expr EQ e2=expr  {( EEq (e1,e2) ,($startpos,$endpos))}
  | e1=expr LT e2=expr  {( ELess (e1,e2) ,($startpos,$endpos))}
  | e=expr PLUSUN  {( ESucc e ,($startpos,$endpos))}
  | e1=expr AND e2=expr  {( EAnd (e1,e2) ,($startpos,$endpos))}


ident:
  | s=IDENT  {( s ,($startpos,$endpos))}

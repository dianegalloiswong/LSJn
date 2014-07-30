%{

  open Machine_abstraite

%}

%token LDA LDC LDN LDV SAVE STV RST UJP SKIP PAIR SPLIT SWAP ISNULL ISINT SUCC PRED LEQ HALT
%token SEMICOLON
%token <int> INT
%token EOF

%start prog

%type <Machine_abstraite.instr list> prog


%%


prog:
  | l=instr_block* EOF  { l }

instr_block:
  | i=instr SEMICOLON  { i }

instr:
  | LDA n=INT  { LDA n }
  | LDC n=INT  { LDC n }
  | LDN  { LDN }
  | LDV n=INT  { LDV n }
  | SAVE n=INT  { SAVE n }
  | STV n=INT  { STV n }
  | RST n=INT  { RST n }
  | UJP { UJP }
  | SKIP  { SKIP }
  | PAIR  { PAIR }
  | SPLIT  { SPLIT }
  | SWAP  { SWAP }
  | ISNULL  { ISNULL }
  | ISINT  { ISINT }
  | SUCC  { SUCC }
  | PRED  { PRED }
  | LEQ  { LEQ }
  | HALT  { HALT }

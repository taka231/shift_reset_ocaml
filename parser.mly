%{
  open Syntax
  (* ここに書いたものは，ExampleParser.mliに入らないので注意 *)
%}

%token <int>    INT
%token <bool>   BOOL
%token <string> ID
%token LET IN LETAND
%token PLUS TIMES MINUS DIV
%token EQ LT
%token AND OR
%token IF THEN ELSE
%token LPAR RPAR
%token FUN ARROW
%token REC
%token LBRACKET RBRACKET CONS COMMA
%token MATCH WITH BAR
%token SEMISEMI
%token RESET SHIFT

%start toplevel
%type <Syntax.command list> toplevel
%%

toplevel:
  | expr SEMISEMI { [CExp $1] }
  | lettop        { $1 }
;

lettop:
  | LET var EQ expr lettop { (CDecl ($2, $4))::$5 }
  | LET REC var var EQ expr after=lettop { (CRecDecl ($3,$4,$6))::after }
  | SEMISEMI { [] }

expr:
  | or_expr                  { $1 }
  | FUN var ARROW expr          { EFun($2,$4) }
  | MATCH expr WITH cases           { EMatch($2, $4) }
  | MATCH expr WITH BAR cases       { EMatch($2, $5) }
  | other_expr               { $1 }

cases:
  | pattern ARROW expr           { [($1, $3)] }
  | pattern ARROW expr BAR cases { ($1, $3) :: $5 }
;

pattern:
  | atomic_pattern CONS pattern      { PCons($1,$3) }
  | atomic_pattern                   { $1 }
;

atomic_pattern:
  | INT                              { PInt($1) }
  | BOOL                             { PBool($1) }
  | var                              { PVar($1) }
  | LPAR pattern COMMA pattern RPAR  { PPair($2, $4) }
  | LBRACKET RBRACKET                { PNil }
  | LPAR pattern RPAR                { $2 }
;

or_expr:
  | and_expr OR or_expr { EOr($1,$3) }
  | and_expr                 { $1 }

and_expr:
  | comp_expr AND and_expr  { EAnd($1,$3) }
  | comp_expr               { $1 }

comp_expr:
  | comp_expr EQ list_expr { EEq($1,$3) }
  | comp_expr LT list_expr { ELt($1,$3) }
  | list_expr               { $1 }

list_expr:
  | arith_expr CONS list_expr { ECons($1, $3) }
  | arith_expr                { $1 }

arith_expr:
  | arith_expr PLUS factor_expr  { EAdd($1,$3) }
  | arith_expr MINUS factor_expr { ESub($1,$3) }
  | factor_expr                  { $1 }

factor_expr:
  | factor_expr TIMES app_expr { EMul($1,$3) }
  | factor_expr DIV app_expr   { EDiv($1,$3) }
  | app_expr                   { $1 }
;

app_expr:
  | app_expr atomic_expr { EApp($1, $2) }
  | atomic_expr          { $1 }

other_expr:
  | LET var EQ expr IN expr     { ELet($2, $4, $6) }
  | LET REC var var EQ expr IN expr { ELetRec($3,$4,$6,$8) }
  | IF expr THEN expr ELSE expr { EIf($2,$4,$6) }

atomic_expr:
  | INT            { EConstInt($1) }
  | BOOL           { EConstBool($1) }
  | ID             { EVar($1) }
  | RESET LPAR expr RPAR    { EReset($3) }
  | SHIFT var LPAR expr RPAR { EShift($2,$4) }
  | LPAR RPAR      { EUnit }
  | LPAR expr COMMA expr RPAR { EPair($2,$4) }
  | LBRACKET RBRACKET { ENil }
  | LPAR expr RPAR { $2 }
;


var:
  | ID { $1 }
;

%{
(* See this for a tutorial on ocamlyacc
 * http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/ *)
open Nano

let rec consAtTheEnd l e = match l with
  | NilExpr       -> Bin (e, Cons, NilExpr)
  | Bin(h, op, t) -> Bin (h, op,   consAtTheEnd t e)

%}

%token <int> Num
%token <string> Id
%token EOF LET EQ IN FUN
%token PLUS MUL AND OR
%token LPAREN RPAREN
%token SEMI COLONCOLON
%token TRUE FALSE
%token LET REC IN ARROW
%token IF THEN ELSE
%token PLUS MINUS
%token MUL DIV
%token LT LE NE
%token AND OR
%token LBRAC RBRAC

%left MUL DIV PLUS MINUS EQ NE LT LE AND OR FUN APP

%start exp
%type <Nano.expr> exp

%%

exp       : exp8                       { $1 }

exp8      : LET REC Id EQ exp8 IN exp8   { Letrec($3,$5,$7) }
          | LET Id EQ exp8 IN exp8       { Let($2,$4,$6) }
          | FUN Id ARROW exp8 				   { Fun ($2,$4) }
          | IF exp8 THEN exp8 ELSE exp8  { If ($2,$4,$6) }
          | exp7                         { $1 }

exp7      : exp7 OR exp6               { Bin($1,Or,$3) }
          | exp6                       { $1 }

exp6      : exp6 AND exp5              { Bin($1,And,$3) }
          | exp5                       { $1 }

exp5      : exp5 EQ exp54              { Bin($1,Eq,$3) }
          | exp5 NE exp54              { Bin($1,Ne,$3) }
          | exp5 LT exp54              { Bin($1,Lt,$3) }
          | exp5 LE exp54              { Bin($1,Le,$3) }
          | exp54                      { $1 }

exp54     : exp4 COLONCOLON exp54      { Bin($1,Cons,$3) }
          | LBRAC expseq RBRAC         { $2 }
          | exp4                       { $1 }

exp4      : exp4 PLUS exp3             { Bin($1,Plus,$3) }
          | exp4 MINUS exp3            { Bin($1,Minus,$3) }
          | exp3                       { $1 }

exp3      : exp3 MUL exp2              { Bin($1,Mul,$3) }
          | exp3 DIV exp2              { Bin($1,Div,$3) }
          | exp2                       { $1 }

exp2      : exp2 exp1                  { App($1,$2) }
          | exp1                       { $1 }

exp1      : Num                        { Const $1 }
          | TRUE                       { True }
          | FALSE                      { False }
          | Id                         { Var $1 }
          | LPAREN exp RPAREN          { $2 }
          | LBRAC RBRAC                { NilExpr }


expseq    : expseq SEMI exp            { consAtTheEnd $1      $3 }
          | exp                        { consAtTheEnd NilExpr $1 }

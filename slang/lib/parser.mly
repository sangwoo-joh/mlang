/* SNU 4190.310 Programming Languages */

%{
exception EmptyBinding
let rec desugarLet =
  function ([], _) -> raise EmptyBinding
   | (a::[], e) -> S.Let(a,e)
   | (a::r, e) -> S.Let(a, desugarLet(r,e))

exception IncorrectSelection
let whichSel = function (e, 1) -> S.Fst e
       | (e, 2) -> S.Snd e
       | _ -> raise IncorrectSelection
%}
%token TRUE FALSE AND OR IF THEN ELSE LET IN END FN READ WRITE RARROW EQUAL
%token PLUS MINUS LP RP VAL COLONEQ BANG MALLOC SEMICOLON REC EOF DOT COMMA
%token <int> NUM
%token <string> ID
%token <string> STRING

%left SEMICOLON
%right FN RARROW LET
%right WRITE
%right COLONEQ
%nonassoc IF THEN ELSE
%left EQUAL
%left PLUS MINUS OR
%left AND
%right BANG MALLOC
%left DOT
%nonassoc TRUE FALSE NUM ID STRING READ LP

%start program
%type <S.exp> program
%type <S.exp> expr
%type <S.decl> decl

%%
program: expr EOF   {$1}
    ;
expr: aexpr {$1}
    | expr aexpr {S.App($1,$2)}
    | expr PLUS expr {S.Bop(S.Add,$1,$3)}
    | expr MINUS expr {S.Bop(S.Sub,$1,$3)}
    | expr EQUAL expr {S.Bop(S.Eq,$1,$3)}
    | expr AND expr {S.Bop(S.And,$1,$3)}
    | expr OR expr {S.Bop(S.Or,$1,$3)}
    | expr SEMICOLON expr {S.Seq ($1,$3)}
    | expr COLONEQ expr {S.Assign($1,$3)}
    | expr DOT NUM {whichSel ($1,$3)}
    ;
aexpr: LP expr RP {$2}
    | NUM {S.Const(S.Nat $1)}
    | STRING {S.Const(S.Str $1)}
    | TRUE {S.Const(S.Bool true)}
    | FALSE {S.Const(S.Bool false)}
    | ID {S.Var($1)}
    | READ {S.Read}
    | FN ID RARROW expr {S.Fn($2,$4)}
    | LET decls IN expr END {desugarLet($2,$4)}
    | IF expr THEN expr ELSE expr {S.If($2,$4,$6)}
    | WRITE expr {S.Write ($2)}
    | MALLOC expr {S.Malloc ($2)}
    | BANG expr {S.Bang ($2)}
    | LP expr COMMA expr RP {S.Pair ($2,$4)}
    ;
decls: decl {[$1]}
    | decls decl {$1 @ [$2]}
    ;
decl: VAL ID EQUAL expr {S.Val($2, $4)}
    | REC ID EQUAL FN ID RARROW expr {S.Rec($2, $5, $7)}
    ;
%%

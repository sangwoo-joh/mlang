/* SNU 4190.310 Programming Languages */

%{
exception EmptyBinding
let rec desugarLet =
  function ([], e) -> raise EmptyBinding
   | (a::[], e) -> M.Let(a,e)
   | (a::r, e) -> M.Let(a, desugarLet(r,e))

exception IncorrectSelection
let whichSel = function (e, 1) -> M.Fst e
       | (e, 2) -> M.Snd e
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
%type <M.exp> program
%type <M.exp> expr
%type <M.decl> decl

%%
program: expr EOF   {$1}
    ;
expr: aexpr {$1}
    | expr aexpr {M.App($1,$2)}
    | expr PLUS expr {M.Bop(M.Add,$1,$3)}
    | expr MINUS expr {M.Bop(M.Sub,$1,$3)}
    | expr EQUAL expr {M.Bop(M.Eq,$1,$3)}
    | expr AND expr {M.Bop(M.And,$1,$3)}
    | expr OR expr {M.Bop(M.Or,$1,$3)}
    | expr SEMICOLON expr {M.Seq ($1,$3)}
    | expr COLONEQ expr {M.Assign($1,$3)}
    | expr DOT NUM {whichSel ($1,$3)}
    ;
aexpr: LP expr RP {$2}
    | NUM {M.Const(M.Nat $1)}
    | STRING {M.Const(M.Str $1)}
    | TRUE {M.Const(M.Bool true)}
    | FALSE {M.Const(M.Bool false)}
    | ID {M.Var($1)}
    | READ {M.Read}
    | FN ID RARROW expr {M.Fn($2,$4)}
    | LET decls IN expr END {desugarLet($2,$4)}
    | IF expr THEN expr ELSE expr {M.If($2,$4,$6)}
    | WRITE expr {M.Write ($2)}
    | MALLOC expr {M.Malloc ($2)}
    | BANG expr {M.Bang ($2)}
    | LP expr COMMA expr RP {M.Pair ($2,$4)}
    ;
decls: decl {[$1]}
    | decls decl {$1 @ [$2]}
    ;
decl: VAL ID EQUAL expr {M.Val($2, $4)}
    | REC ID EQUAL FN ID RARROW expr {M.Rec($2, $5, $7)}
    ;
%%

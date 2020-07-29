%token <int * Lexing.position> NAT
%token <float * Lexing.position> REAL
%token <bool * Lexing.position> BOOL
%token <string * Lexing.position> STR
%token <string * Lexing.position> VAR
%token <Lexing.position> SEMICOLON
%token <Lexing.position> COLONEQ
%token <Lexing.position> READNAT
%token <Lexing.position> READREAL
%token <Lexing.position> READSTR
%token <Lexing.position> WRITE
%token <Lexing.position> ALLOC
%token <Lexing.position> FN
%token <Lexing.position> RIGHTARROW
%token <Lexing.position> LET
%token <Lexing.position> EQ
%token <Lexing.position> REC
%token <Lexing.position> IN
%token <Lexing.position> END
%token <Lexing.position> IF
%token <Lexing.position> THEN
%token <Lexing.position> ELSE
%token <Lexing.position> LEFTPAREN
%token <Lexing.position> RIGHTPAREN
%token <Lexing.position> FST
%token <Lexing.position> SND
%token <Lexing.position> COMMA
%token <Lexing.position> PLUS
%token <Lexing.position> MINUS
%token <Lexing.position> BANG
%token <Lexing.position> NOT
%token <Lexing.position> MULT
%token <Lexing.position> DIV
%token <Lexing.position> AND
%token <Lexing.position> OR
%token <Lexing.position> XOR
%token <Lexing.position> NEQ
%token <Lexing.position> EOF

%left SEMICOLON
%right FN RIGHTARROW LET
%right WRITE
%right COLONEQ
%nonassoc IF ELSE
%left EQ NEQ
%left PLUS MINUS MULT DIV OR XOR
%left AND
%right BANG ALLOC NOT
%right FST SND
%nonassoc BOOL NAT REAL VAR STR READNAT READREAL READSTR LEFTPAREN
%start parse
%type <Exp.t> parse

%%
parse:
    | expr EOF { $1 }
    ;
expr:
  | aexpr { $1 }
  | expr aexpr { Exp.App {e1= $1; e2= $2} }
  | expr PLUS expr { Exp.Bop {op= Exp.Plus; e1= $1; e2= $3} }
  | expr MINUS expr { Exp.Bop {op= Exp.Minus; e1= $1; e2= $3} }
  | expr MULT expr { Exp.Bop {op= Exp.Mult; e1= $1; e2= $3} }
  | expr DIV expr { Exp.Bop {op= Exp.Div; e1= $1; e2= $3} }
  | expr AND expr { Exp.Bop {op= Exp.And; e1= $1; e2= $3} }
  | expr OR expr { Exp.Bop {op= Exp.Or; e1= $1; e2= $3} }
  | expr XOR expr { Exp.Bop {op= Exp.Xor; e1= $1; e2= $3} }
  | expr EQ expr { Exp.Bop {op= Exp.Eq; e1= $1; e2= $3} }
  | expr NEQ expr { Exp.Bop {op= Exp.Neq; e1= $1; e2= $3} }
  | expr SEMICOLON expr { Exp.Seq {e1= $1; e2= $3} }
  | expr COLONEQ expr { Exp.Assign {lhs= $1; rhs= $3} }
  | FST expr { Exp.Fst $2 }
  | SND expr { Exp.Snd $2 }
  ;
aexpr:
  | LEFTPAREN expr RIGHTPAREN { $2 }
  | NAT { Exp.Const (Const.Nat (fst $1)) }
  | REAL { Exp.Const (Const.Real (fst $1)) }
  | BOOL { Exp.Const (Const.Bool (fst $1)) }
  | STR { Exp.Const (Const.Str (fst $1)) }
  | VAR { Exp.Var (fst $1) }
  | READNAT { Exp.Read Exp.ReadNat }
  | READREAL { Exp.Read Exp.ReadReal }
  | READSTR { Exp.Read Exp.ReadStr }
  | FN VAR RIGHTARROW expr { Exp.Lambda {var= (fst $2); body= $4} }
  | LET bind_exp IN expr END { Exp.Let {bind= $2; block= $4} }
  | IF expr THEN expr ELSE expr { Exp.Branch {cond= $2; _then= $4; _else= $6} }
  | WRITE expr { Exp.Write $2 }
  | ALLOC expr { Exp.Alloc $2 }
  | NOT expr { Exp.Uop {op= Exp.Not; exp= $2} }
  | BANG expr { Exp.Uop {op= Exp.Bang; exp= $2} }
  | LEFTPAREN expr COMMA expr RIGHTPAREN { Exp.Pair {fst= $2; snd= $4} }
  ;
bind_exp:
  | VAR EQ expr { Exp.Val {lhs= (fst $1); rhs= $3} }
  | REC VAR EQ expr { Exp.Rec {lhs= (fst $2); rhs= $4} }
  ;
%%

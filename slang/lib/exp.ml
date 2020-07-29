module F = Format

type t =
  | Const of Const.t
  | Var of Var.t
  | Seq of {e1: t; e2: t}
  | Assign of {lhs: t; rhs: t}
  | Read of read
  | Write of t
  | Alloc of t
  | Lambda of {var: Var.t; body: t}
  | Let of {bind: bind; block: t}
  | App of {e1: t; e2: t}
  | Branch of {cond: t; _then: t; _else: t}
  | Uop of {op: op; exp: t}
  | Bop of {op: op; e1: t; e2: t}
  | Pair of {fst: t; snd: t}
  | Fst of t
  | Snd of t
[@@deriving compare, equal]

and bind = Val of {lhs: Var.t; rhs: t} | Rec of {lhs: Var.t; rhs: t} [@@deriving compare, equal]

and op =
  | Plus
  | Minus
  | Mult
  | Div  (** arithmetic operators *)
  | Not
  | And
  | Or
  | Xor  (** boolean operators *)
  | Eq
  | Neq  (** comparison operators *)
  | Bang  (** heap operator *)
[@@deriving compare, equal]

and read = ReadNat | ReadReal | ReadStr [@@deriving compare, equal]

let rec pp fmt = function
  | Const c ->
      F.fprintf fmt "%a" Const.pp c
  | Var v ->
      F.fprintf fmt "%a" Var.pp v
  | Seq {e1; e2} ->
      F.fprintf fmt "%a;@;%a" pp e1 pp e2
  | Assign {lhs; rhs} ->
      F.fprintf fmt "(%a := %a)@," pp lhs pp rhs
  | Read r -> (
    match r with
    | ReadNat ->
        F.fprintf fmt "read_nat()"
    | ReadReal ->
        F.fprintf fmt "read_real()"
    | ReadStr ->
        F.fprintf fmt "read_str()" )
  | Write e ->
      F.fprintf fmt "write(%a)" pp e
  | Alloc e ->
      F.fprintf fmt "alloc(%a)" pp e
  | Lambda {var; body} ->
      F.fprintf fmt "(fn %a -> %a)@," Var.pp var pp body
  | Let {bind; block} ->
      F.fprintf fmt "@[<hv 2>let %a in@;%a@;end@]" pp_bind bind pp block
  | App {e1; e2} ->
      F.fprintf fmt "@[<hv 2>(%a %a)@]" pp e1 pp e2
  | Branch {cond; _then; _else} ->
      F.fprintf fmt "@[<hv 2>if %a then@; %a else@; %a@]" pp cond pp _then pp _else
  | Uop {op; exp} ->
      F.fprintf fmt "%a(%a)" pp_op op pp exp
  | Bop {op; e1; e2} ->
      F.fprintf fmt "(%a %a %a)" pp e1 pp_op op pp e2
  | Pair {fst; snd} ->
      F.fprintf fmt "(%a, %a)" pp fst pp snd
  | Fst e ->
      F.fprintf fmt "(fst %a)" pp e
  | Snd e ->
      F.fprintf fmt "(snd %a)" pp e


and pp_bind fmt = function
  | Val {lhs; rhs} ->
      F.fprintf fmt "%a = %a" Var.pp lhs pp rhs
  | Rec {lhs; rhs} ->
      F.fprintf fmt "rec %a = %a" Var.pp lhs pp rhs


and pp_op fmt = function
  | Plus ->
      F.fprintf fmt "+"
  | Minus ->
      F.fprintf fmt "-"
  | Mult ->
      F.fprintf fmt "*"
  | Div ->
      F.fprintf fmt "/"
  | Not ->
      F.fprintf fmt "not"
  | And ->
      F.fprintf fmt "and"
  | Or ->
      F.fprintf fmt "or"
  | Xor ->
      F.fprintf fmt "xor"
  | Eq ->
      F.fprintf fmt "="
  | Neq ->
      F.fprintf fmt "!="
  | Bang ->
      F.fprintf fmt "!"

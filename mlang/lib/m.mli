type exp =
  | CONST of const
  | VAR of id
  | FN of id * exp
  | APP of exp * exp
  | LET of decl * exp
  | IF of exp * exp * exp
  | BOP of bop * exp * exp
  | READ
  | WRITE of exp
  | MALLOC of exp  (** malloc e *)
  | ASSIGN of exp * exp  (** e := e *)
  | BANG of exp  (** !e *)
  | SEQ of exp * exp  (** e ; e *)
  | PAIR of exp * exp  (** (e, e) *)
  | FST of exp  (** e.1 *)
  | SND of exp

(** e.2 *)
and const = S of string | N of int | B of bool

and id = string

and decl =
  | REC of id * id * exp  (** Recursive function decl. (fun_id, arg_id, body) *)
  | VAL of id * exp

(** Value decl, including non-recursive functions *)
and bop = ADD | SUB | EQ | AND | OR

(** type in M  *)
type typ =
  | TyInt  (** integer type *)
  | TyBool  (** boolean type *)
  | TyString  (** string type *)
  | TyPair of typ * typ  (** pair type *)
  | TyLoc of typ

(** location type *)

(** errors *)
exception RunError of string

exception TypeError of string

val run : exp -> unit

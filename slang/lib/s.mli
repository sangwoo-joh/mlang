type exp =
  | Const of const
  | Var of id
  | Fn of id * exp
  | App of exp * exp
  | Let of decl * exp
  | If of exp * exp * exp
  | Bop of bop * exp * exp
  | Read
  | Write of exp
  | Malloc of exp
  | Assign of exp * exp
  | Bang of exp
  | Seq of exp * exp
  | Pair of exp * exp
  | Fst of exp
  | Snd of exp

and const = Str of string | Nat of int | Bool of bool

and id = string

and decl =
  | Rec of id * id * exp  (** Recursive function decl. (fun_id, arg_id, body) *)
  | Val of id * exp  (** Value decl, including non-recursive functions *)

and bop = Add | Sub | Eq | And | Or

type typ =
  | TInt  (** integer type *)
  | TBool  (** boolean type *)
  | TString  (** string type *)
  | TPair of typ * typ  (** pair type *)
  | TLoc of typ  (** memory address *)

(** errors *)
exception RunError of string

exception TypeError of string

val run : exp -> unit

(** SNU 4190.310 Programming Languages *)

(** Definition of M's syntax, type and interpreter *)
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

(** type in M  *)
type typ =
  | TInt  (** integer type *)
  | TBool  (** boolean type *)
  | TString  (** string type *)
  | TPair of typ * typ  (** pair type *)
  | TLoc of typ

(** location type *)

(** errors *)
exception RunError of string

exception TypeError of string

(** domains *)
type loc = int

type value =
  | VInt of int
  | VString of string
  | VBool of bool
  | VLoc of loc
  | VPair of value * value
  | VClosure of closure

and closure = fexpr * env

and fexpr = Fun of id * exp | RecFun of id * id * exp

and env = id -> value

type memory = int * (loc -> value)

(** notations (see 5 page in M.pdf)
   * f @+ (x, v)              f[x |-> v]
   * store M (l, v)           M[l |-> v]
   * load M l                M(l)
   *)
let loc_count = ref 0

let ( @+ ) f (x, v) y = if y = x then v else f y

let store m (l, v) = m @+ (l, v)

let load m l = m l

let bind env (x, v) = env @+ (x, v)

let malloc m =
  loc_count := !loc_count + 1 ;
  (!loc_count, m)


(** auxiliary functions *)
let getInt = function VInt n -> n | _ -> raise (TypeError "not an int")

let getString = function VString s -> s | _ -> raise (TypeError "not a string")

let getBool = function VBool b -> b | _ -> raise (TypeError "not a bool")

let getLoc = function VLoc l -> l | _ -> raise (TypeError "not a loc")

let getPair = function VPair (a, b) -> (a, b) | _ -> raise (TypeError "not a pair")

let getClosure = function VClosure c -> c | _ -> raise (TypeError "not a function")

let op2fn = function
  | Add ->
      fun (v1, v2) -> VInt (getInt v1 + getInt v2)
  | Sub ->
      fun (v1, v2) -> VInt (getInt v1 - getInt v2)
  | And ->
      fun (v1, v2) -> VBool (getBool v1 && getBool v2)
  | Or ->
      fun (v1, v2) -> VBool (getBool v1 || getBool v2)
  | Eq -> (
      fun (v1, v2) ->
        match (v1, v2) with
        | VInt n1, VInt n2 ->
            VBool (n1 = n2)
        | VString s1, VString s2 ->
            VBool (s1 = s2)
        | VBool b1, VBool b2 ->
            VBool (b1 = b2)
        | VLoc l1, VLoc l2 ->
            VBool (l1 = l2)
        | _ ->
            raise (TypeError "EQ operands are not int/bool/str/loc") )


let rec printValue = function
  | VInt n ->
      print_endline (string_of_int n)
  | VBool b ->
      print_endline (string_of_bool b)
  | VString s ->
      print_endline s
  | _ ->
      raise (TypeError "WRITE operand is not int/bool/string")


let rec eval env mem exp =
  match exp with
  | Const (Str s) ->
      (VString s, mem)
  | Const (Nat n) ->
      (VInt n, mem)
  | Const (Bool b) ->
      (VBool b, mem)
  | Var x ->
      (env x, mem)
  | Fn (x, e) ->
      (VClosure (Fun (x, e), env), mem)
  | App (e1, e2) -> (
      let v1, m' = eval env mem e1 in
      let v2, m'' = eval env m' e2 in
      let c, env' = getClosure v1 in
      match c with
      | Fun (x, e) ->
          eval (bind env' (x, v2)) m'' e
      | RecFun (f, x, e) ->
          let env'' = bind env' (x, v2) in
          let env''' = bind env'' (f, v1) in
          eval env''' m'' e )
  | If (e1, e2, e3) ->
      let v1, m' = eval env mem e1 in
      eval env m' (if getBool v1 then e2 else e3)
  | Bop (op, e1, e2) ->
      let v1, m' = eval env mem e1 in
      let v2, m'' = eval env m' e2 in
      ((op2fn op) (v1, v2), m'')
  | Read ->
      let n = try read_int () with _ -> raise (RunError "read error") in
      (VInt n, mem)
  | Write e ->
      let v, m' = eval env mem e in
      let _ = printValue v in
      (v, m')
  | Pair (e1, e2) ->
      let v1, m' = eval env mem e1 in
      let v2, m'' = eval env m' e2 in
      (VPair (v1, v2), m'')
  | Fst e ->
      let v, m' = eval env mem e in
      (fst (getPair v), m')
  | Snd e ->
      let v, m' = eval env mem e in
      (snd (getPair v), m')
  | Seq (e1, e2) ->
      let v, m' = eval env mem e1 in
      eval env m' e2
  | Let (Val (x, e1), e2) ->
      let v1, m' = eval env mem e1 in
      eval (bind env (x, v1)) m' e2
  | Let (Rec (f, x, e1), e2) ->
      let closure = VClosure (RecFun (f, x, e1), env) in
      eval (bind env (f, closure)) mem e2
  | Malloc e ->
      let v, m' = eval env mem e in
      let l, m'' = malloc m' in
      (VLoc l, store m'' (l, v))
  | Assign (e1, e2) ->
      let v1, m' = eval env mem e1 in
      let v2, m'' = eval env m' e2 in
      (v2, store m'' (getLoc v1, v2))
  | Bang e ->
      let v, m' = eval env mem e in
      (load m' (getLoc v), m')


let emptyEnv x = raise (RunError ("unbound id: " ^ x))

let emptyMem l = raise (RunError ("uninitialized loc: " ^ string_of_int l))

let run exp = ignore (eval emptyEnv emptyMem exp)

module F = Format
open Core

exception RuntimeError of string

(** heap location *)
module Loc = struct
  type t = int [@@deriving compare, equal, sexp]

  let hash = Hashtbl.hash

  let base : t = 0

  let pp fmt t = F.fprintf fmt "%#x" t
end

[@@@warning "-37"]

module rec Env : sig
  type t

  val pp : F.formatter -> t -> unit

  val empty : t

  val bind : t -> Var.t -> Value.t -> t

  val get : t -> Var.t -> Value.t
end = struct
  include Map.Make (Var)

  type nonrec t = Value.t t

  let pp fmt env =
    let pp_map ~key ~data fmt = F.fprintf fmt "%a  |----->  %a@;" Var.pp key Value.pp data in
    iteri env ~f:(pp_map fmt)


  let bind env x v = set env ~key:x ~data:v

  let get env x =
    match find env x with
    | Some v ->
        v
    | None ->
        raise (RuntimeError (F.asprintf "Unbound variable: %a@." Var.pp x))
end

(** closure: env with function abstraction *)
and Closure : sig
  type fexp = Fun of Exp.t | RecFun of Var.t * Exp.t * int

  type t = {env: Env.t; fexp: fexp}

  val pp_fexp : F.formatter -> fexp -> unit

  val pp : F.formatter -> t -> unit

  val of_fun : Env.t -> Exp.t -> Closure.t

  val of_rec : Env.t -> fname:Var.t -> depth:int -> Exp.t -> Closure.t

  val max_recursion : int

  val next_call : t -> t
end = struct
  type fexp = Fun of Exp.t | RecFun of Var.t * Exp.t * int

  type t = {env: Env.t; fexp: fexp}

  let max_recursion = 256

  let next_call t =
    match t.fexp with
    | Fun _ ->
        t
    | RecFun (var, exp, depth) ->
        {t with fexp= RecFun (var, exp, depth + 1)}


  let of_fun env lambda =
    let open Exp in
    match lambda with
    | Lambda _ ->
        {env; fexp= Fun lambda}
    | _ as e ->
        raise (RuntimeError (F.asprintf "Not a closure: %a@." Exp.pp e))


  let of_rec env ~fname ~depth lambda =
    let open Exp in
    match lambda with
    | Lambda _ ->
        {env; fexp= RecFun (fname, lambda, depth)}
    | _ as e ->
        raise (RuntimeError (F.asprintf "Not a closure: %a@." Exp.pp e))


  let pp_fexp fmt = function
    | Fun e ->
        Exp.pp fmt e
    | RecFun (f, e, depth) ->
        F.fprintf fmt "rec (%i) %a:%a" depth Var.pp f Exp.pp e


  let pp fmt {env; fexp} = F.fprintf fmt "<closure> (env: %a) (%a)" Env.pp env pp_fexp fexp
end

(** value: caculated value *)
and Value : sig
  type t = Const of Const.t | Loc of Loc.t | Pair of t * t | Closure of Closure.t

  val pp : F.formatter -> t -> unit

  val of_const : Const.t -> t

  val of_loc : Loc.t -> t

  val of_pair : t -> t -> t

  val of_closure : Closure.t -> t

  val fst_of : t -> t

  val snd_of : t -> t

  val int_of : t -> int

  val float_of : t -> float

  val bool_of : t -> bool

  val str_of : t -> string

  val loc_of : t -> Loc.t

  val closure_of : t -> Closure.t
end = struct
  type t = Const of Const.t | Loc of Loc.t | Pair of t * t | Closure of Closure.t

  let rec pp fmt = function
    | Const c ->
        Const.pp fmt c
    | Loc l ->
        Loc.pp fmt l
    | Pair (v1, v2) ->
        F.fprintf fmt "(%a, %a)" pp v1 pp v2
    | Closure c ->
        Closure.pp fmt c


  let of_const c = Const c

  let of_loc l = Loc l

  let of_pair fst snd = Pair (fst, snd)

  let of_closure c = Closure c

  let fst_of = function
    | Pair (f, _) ->
        f
    | _ as v ->
        raise (RuntimeError (F.asprintf "fst: Not a pair: %a@." pp v))


  let snd_of = function
    | Pair (_, s) ->
        s
    | _ as v ->
        raise (RuntimeError (F.asprintf "snd: Not a pair: %a@." pp v))


  let int_of = function
    | Const (Const.Nat i) ->
        i
    | _ as v ->
        raise (RuntimeError (F.asprintf "Not an integer: %a@." pp v))


  let float_of = function
    | Const (Const.Real r) ->
        r
    | _ as v ->
        raise (RuntimeError (F.asprintf "Not a float: %a@." pp v))


  let bool_of = function
    | Const (Const.Bool b) ->
        b
    | _ as v ->
        raise (RuntimeError (F.asprintf "Not a boolean: %a@." pp v))


  let str_of = function
    | Const (Const.Str s) ->
        s
    | _ as v ->
        raise (RuntimeError (F.asprintf "Not a string: %a@." pp v))


  let loc_of = function
    | Loc l ->
        l
    | _ as v ->
        raise (RuntimeError (F.asprintf "Not a valid location: %a@." pp v))


  let closure_of = function
    | Closure c ->
        c
    | _ as v ->
        raise (RuntimeError (F.asprintf "Not a closure: %a@." pp v))
end

module Memory = struct
  include Hashtbl.Make (Loc)

  type nonrec t = Value.t t

  (** location related APIs *)
  let base : Loc.t ref = ref Loc.base

  let malloc () =
    Stdlib.incr base ;
    !base


  let empty : t = create ()

  let pp fmt m =
    let pp_map ~key ~data fmt = F.fprintf fmt "%a  ----->  %a@;" Loc.pp key Value.pp data in
    iteri m ~f:(pp_map fmt)


  (** store: always overwrite the existing data *)
  let store m l v = set m ~key:l ~data:v

  (** load: runtime error when try to access invalid location *)
  let load m l =
    match find m l with
    | Some v ->
        v
    | None ->
        raise (RuntimeError (F.asprintf "Invalid location: %a@." Loc.pp l))
end

let eval_bop ~op c1 c2 =
  let open Const in
  match op with
  | Exp.Plus -> (
    match (c1, c2) with
    | Nat n1, Nat n2 ->
        Nat (n1 + n2)
    | Nat n1, Real r2 ->
        Nat (n1 + int_of_float r2)
    | Real r1, Real r2 ->
        Real (r1 +. r2)
    | Real r1, Nat n2 ->
        Real (r1 +. float_of_int n2)
    | Str s1, Str s2 ->
        Str (s1 ^ s2)
    | _ ->
        raise
          (Typ.TypeError
             (F.asprintf "Type error: not allowed to %a %a %a@." pp c1 Exp.pp_op op pp c2)) )
  | Exp.Minus -> (
    match (c1, c2) with
    | Nat n1, Nat n2 ->
        Nat (n1 - n2)
    | Nat n1, Real r2 ->
        Nat (n1 - int_of_float r2)
    | Real r1, Real r2 ->
        Real (r1 -. r2)
    | Real r1, Nat n2 ->
        Real (r1 -. float_of_int n2)
    | _ ->
        raise
          (Typ.TypeError
             (F.asprintf "Type error: not allowed to %a %a %a@." pp c1 Exp.pp_op op pp c2)) )
  | Exp.Mult -> (
    match (c1, c2) with
    | Nat n1, Nat n2 ->
        Nat (n1 * n2)
    | Nat n1, Real r2 ->
        Nat (n1 * int_of_float r2)
    | Real r1, Real r2 ->
        Real (r1 *. r2)
    | Real r1, Nat n2 ->
        Real (r1 *. float_of_int n2)
    | _ ->
        raise
          (Typ.TypeError
             (F.asprintf "Type error: not allowed to %a %a %a@." pp c1 Exp.pp_op op pp c2)) )
  | Exp.Div -> (
    match (c1, c2) with
    | Nat n1, Nat n2 ->
        if n2 = 0 then raise (RuntimeError "Division by zero@.") ;
        Nat (n1 / n2)
    | Nat n1, Real r2 ->
        if Float.equal r2 0. then raise (RuntimeError "Division by zero@.") ;
        Nat (n1 / int_of_float r2)
    | Real r1, Real r2 ->
        if Float.equal r2 0. then raise (RuntimeError "Division by zero@.") ;
        Real (r1 /. r2)
    | Real r1, Nat n2 ->
        if n2 = 0 then raise (RuntimeError "Division by zero@.") ;
        Real (r1 /. float_of_int n2)
    | _ ->
        raise
          (Typ.TypeError
             (F.asprintf "Type error: not allowed to %a %a %a@." pp c1 Exp.pp_op op pp c2)) )
  | Exp.Eq -> (
    match (c1, c2) with
    | Nat n1, Nat n2 ->
        Bool (n1 = n2)
    | Nat n1, Real r2 ->
        Bool (n1 = int_of_float r2)
    | Real r1, Real r2 ->
        Bool Float.(r1 =. r2)
    | Real r1, Nat n2 ->
        Bool Float.(r1 =. float_of_int n2)
    | Bool b1, Bool b2 ->
        Bool Bool.(b1 = b2)
    | Str s1, Str s2 ->
        Bool String.(s1 = s2)
    | _ ->
        raise
          (Typ.TypeError
             (F.asprintf "Type error: not allowed to %a %a %a@." pp c1 Exp.pp_op op pp c2)) )
  | Exp.Neq -> (
    match (c1, c2) with
    | Nat n1, Nat n2 ->
        Bool (n1 <> n2)
    | Nat n1, Real r2 ->
        Bool (n1 <> int_of_float r2)
    | Real r1, Real r2 ->
        Bool Float.(r1 <>. r2)
    | Real r1, Nat n2 ->
        Bool Float.(r1 <> float_of_int n2)
    | Bool b1, Bool b2 ->
        Bool Bool.(b1 <> b2)
    | Str s1, Str s2 ->
        Bool String.(s1 <> s2)
    | _ ->
        raise
          (Typ.TypeError
             (F.asprintf "Type error: not allowed to %a %a %a@." pp c1 Exp.pp_op op pp c2)) )
  | Exp.And -> (
    match (c1, c2) with
    | Bool b1, Bool b2 ->
        Bool (b1 && b2)
    | _ ->
        raise
          (Typ.TypeError
             (F.asprintf "Type error: not allowed to %a %a %a@." pp c1 Exp.pp_op op pp c2)) )
  | Exp.Or -> (
    match (c1, c2) with
    | Bool b1, Bool b2 ->
        Bool (b1 || b2)
    | _ ->
        raise
          (Typ.TypeError
             (F.asprintf "Type error: not allowed to %a %a %a@." pp c1 Exp.pp_op op pp c2)) )
  | Exp.Xor -> (
    match (c1, c2) with
    | Bool b1, Bool b2 ->
        Bool Bool.(b1 <> b2)
    | _ ->
        raise
          (Typ.TypeError
             (F.asprintf "Type error: not allowed to %a %a %a@." pp c1 Exp.pp_op op pp c2)) )
  | Exp.Bang | Exp.Not ->
      raise
        (Typ.TypeError (F.asprintf "Type error: not allowed to %a %a %a@." pp c1 Exp.pp_op op pp c2))


let rec eval : Env.t -> Memory.t -> Exp.t -> Value.t * Memory.t =
 fun env mem exp ->
  match exp with
  | Const const ->
      (Value.of_const const, mem)
  | Var var ->
      (* stack value *)
      (Env.get env var, mem)
  | Seq {e1; e2} ->
      let _, mem' = eval env mem e1 in
      eval env mem' e2
  | Assign {lhs; rhs} ->
      (* store to the heap memory *)
      let lv, lm = eval env mem lhs in
      let loc = Value.loc_of lv in
      let rv, rm = eval env lm rhs in
      Memory.store rm loc rv ;
      (rv, rm)
  | Read read ->
      Out_channel.(flush stdout) ;
      let v =
        let open Exp in
        match read with
        | ReadNat ->
            Value.of_const (Const.Nat (Int.of_string In_channel.(input_line_exn stdin)))
        | ReadReal ->
            Value.of_const (Const.Real (Float.of_string In_channel.(input_line_exn stdin)))
        | ReadStr ->
            Value.of_const (Const.Str In_channel.(input_line_exn stdin))
      in
      (v, mem)
  | Write t ->
      (* polymorphic write *)
      let v, mem' = eval env mem t in
      F.fprintf F.std_formatter "@[<hov 2>%a@]@." Value.pp v ;
      (v, mem')
  | Alloc t ->
      (* alloc fresh location *)
      let loc = Memory.malloc () in
      let v, mem' = eval env mem t in
      Memory.store mem' loc v ;
      (Value.of_loc loc, mem')
  | Lambda _ as lambda ->
      let closure = Closure.of_fun env lambda in
      (Value.of_closure closure, mem)
  | Let {bind; block} ->
      let open Exp in
      let env', mem' =
        match bind with
        | Val {lhs; rhs} ->
            (* non-recursive let-binding *)
            let rv, mem' = eval env mem rhs in
            (Env.bind env lhs rv, mem')
        | Rec {lhs; rhs} ->
            (* recursive let-binding *)
            let closure = Closure.of_rec env ~fname:lhs ~depth:0 rhs in
            (Env.bind env lhs (Value.of_closure closure), mem)
      in
      eval env' mem' block
  | App {e1; e2} -> (
      let v1, m1 = eval env mem e1 in
      let v2, m2 = eval env m1 e2 in
      let closure = Value.closure_of v1 in
      match closure.fexp with
      | Closure.Fun (Exp.Lambda {var; body}) ->
          (* non-recursive function call *)
          (* just bind [v2] as function argument, and eval [body] of closure *)
          let env' = Env.bind closure.env var v2 in
          eval env' m2 body
      | Closure.RecFun (fname, Exp.Lambda {var; body}, depth) ->
          (* recursive function call *)
          (* check stackoverflow *)
          if depth > Closure.max_recursion then
            raise (RuntimeError "Stack overflow: exceeded max recursion") ;
          (* 1. bind [v2] as function argument, as non-recursive function does *)
          let env' = Env.bind closure.env var v2 in
          (* 2. set recursive function call with new depth *)
          let env'' = Env.bind env' fname (Value.of_closure (Closure.next_call closure)) in
          eval env'' m2 body
      | _ as nonfun ->
          raise
            (RuntimeError (F.asprintf "Application: not a function: %a@." Closure.pp_fexp nonfun)) )
  | Branch {cond; _then; _else} ->
      let vcond, mem' = eval env mem cond in
      eval env mem' (if Value.bool_of vcond then _then else _else)
  | Uop {op; exp} ->
      let v, mem' = eval env mem exp in
      let v' =
        Exp.(
          match op with
          | Not ->
              Value.of_const (Const.Bool (not (Value.bool_of v)))
          | Bang ->
              (* load from heap memory *)
              let loc = Value.loc_of v in
              Memory.load mem' loc
          | _ as op ->
              raise (RuntimeError (F.asprintf "Invalid unary operation: %a@." Exp.pp_op op)))
      in
      (v', mem')
  | Bop {op; e1; e2} -> (
      let v1, m1 = eval env mem e1 in
      let v2, m2 = eval env m1 e2 in
      match (v1, v2) with
      | Value.Const c1, Value.Const c2 ->
          (Value.Const (eval_bop ~op c1 c2), m2)
      | _, _ ->
          raise
            (Typ.TypeError
               (F.asprintf "Not allowed to %a %a %a@." Value.pp v1 Exp.pp_op op Value.pp v2)) )
  | Pair {fst; snd} ->
      let v1, mem' = eval env mem fst in
      let v2, mem'' = eval env mem' snd in
      (Value.of_pair v1 v2, mem'')
  | Fst t ->
      let v, mem' = eval env mem t in
      (Value.fst_of v, mem')
  | Snd t ->
      let v, mem' = eval env mem t in
      (Value.snd_of v, mem')


let run : Exp.t -> Value.t * Memory.t = fun exp -> eval Env.empty Memory.empty exp

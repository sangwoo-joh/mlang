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
    let pp_map ~key ~data fmt = F.fprintf fmt "%a |-> %a@;" Var.pp key Value.pp data in
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
  type t

  val pp : F.formatter -> t -> unit

  val of_fun : ?recursive:bool -> Env.t -> Exp.t -> Closure.t
end = struct
  type fexp = Fun of Exp.t | RecFun of Exp.t

  type t = {env: Env.t; fexp: fexp}

  let of_fun ?(recursive = false) env lambda =
    let open Exp in
    match lambda with
    | Lambda _ ->
        if recursive then {env; fexp= RecFun lambda} else {env; fexp= Fun lambda}
    | _ as e ->
        raise (RuntimeError (F.asprintf "Not a closure: %a@." Exp.pp e))


  let pp fmt {env; fexp} =
    let pp_fexp fmt = function
      | Fun e ->
          Exp.pp fmt e
      | RecFun e ->
          F.fprintf fmt "rec %a" Exp.pp e
    in
    F.fprintf fmt "<closure> (env: %a) (%a)" Env.pp env pp_fexp fexp
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
    let pp_map ~key ~data fmt = F.fprintf fmt "%a -> %a@;" Loc.pp key Value.pp data in
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

[@@@warning "-27-32"]

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
      F.fprintf F.std_formatter "@[<hov 2%a@]@." Value.pp v ;
      (v, mem')
  | Alloc t ->
      (* alloc fresh location *)
      let loc = Memory.malloc () in
      let v, mem' = eval env mem t in
      Memory.store mem' loc v ;
      (Value.of_loc loc, mem')
  | Lambda {var; body} as lambda ->
      let closure = Closure.of_fun env lambda ~recursive:false in
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
            let closure = Closure.of_fun env rhs ~recursive:true in
            (Env.bind env lhs (Value.of_closure closure), mem)
      in
      eval env' mem' block
  | App {e1; e2} ->
      failwith "TODO"
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
  | Bop {op; e1; e2} ->
      failwith "TODO"
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

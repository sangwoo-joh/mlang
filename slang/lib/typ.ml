open Core
module F = Format

type id = string [@@deriving compare, equal]

type t =
  | TVar of id  (** Type variable *)
  | TInt  (** integer type *)
  | TBool  (** boolean type *)
  | TString  (** string type *)
  | TFloat  (** floatint point type *)
  | TPair of t * t  (** pair *)
  | TLoc of t  (** pointer type *)
  | TLambda of t * t  (** lambda abstraction; t -> t *)
[@@deriving compare, equal]

exception TypeError of string

let rec pp fmt = function
  | TVar id ->
      F.fprintf fmt "'%s" id
  | TInt ->
      F.fprintf fmt "int"
  | TBool ->
      F.fprintf fmt "bool"
  | TString ->
      F.fprintf fmt "string"
  | TFloat ->
      F.fprintf fmt "float"
  | TPair (t1, t2) ->
      F.fprintf fmt "(%a, %a)" pp t1 pp t2
  | TLoc t ->
      F.fprintf fmt "loc(%a)" pp t
  | TLambda (t1, t2) ->
      F.fprintf fmt "(%a -> %a)" pp t1 pp t2

open Core
module F = Format

type t = Nat of int | Real of float | Bool of bool | Str of string [@@deriving compare, equal]

let pp fmt = function
  | Nat i ->
      F.fprintf fmt "%i" i
  | Real f ->
      F.fprintf fmt "%f" f
  | Bool b ->
      F.fprintf fmt "%b" b
  | Str s ->
      F.fprintf fmt "%s" s

open Core
module F = Format

type t = string [@@deriving compare, equal, sexp]

let pp fmt (t : t) = F.fprintf fmt "%s" t

let hash = String.hash

open Core
module F = Format

type t = string [@@deriving compare, equal]

let pp fmt (t : t) = F.fprintf fmt "%s" t

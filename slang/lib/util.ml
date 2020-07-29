module F = Format

let pp_pos fmt Lexing.{pos_fname; pos_lnum; pos_cnum; pos_bol} =
  F.fprintf fmt "%s:%i:%i" pos_fname pos_lnum (pos_cnum - pos_bol)


let str_of_pos Lexing.{pos_fname; pos_lnum; pos_cnum; pos_bol} =
  F.sprintf "%s:%i:%i" pos_fname pos_lnum (pos_cnum - pos_bol)

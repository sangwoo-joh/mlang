{
open Lexing
open Parser

exception LexingError of string

module F = Format

let pos lexbuf = lexbuf.lex_start_p

let comment_depth = ref 0
}

let whitespace = [' ' '\t']
let newline = '\n' | "\r\n"
let identifier = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '\'' '0'-'9' '_']*
let digit = ['0'-'9']
let nonzerodigit = ['1'-'9']
let intpart = digit+
let fraction = '.' digit+
let decimal = nonzerodigit digit* | '0'
let floatingpoint = intpart? fraction | intpart '.'
let stringliteral = '"' [^ '"' ]* '"'


rule token = parse
  | whitespace+
    { token lexbuf } (** consume blanks *)
  | newline+
    { token lexbuf } (** consume blanks *)
  | decimal as n
    { NAT (int_of_string n, pos lexbuf) }
  | floatingpoint as f
    { REAL (float_of_string f, pos lexbuf) }
  | stringliteral as s
    { STR (s, pos lexbuf) }
  | identifier as id
    { let cursor = pos lexbuf in
      (** keywords *)
      match id with
      | "true" -> BOOL (true, cursor)
      | "false" -> BOOL (false, cursor)
      | "read_nat" -> READNAT cursor
      | "read_real" -> READREAL cursor
      | "read_str" -> READSTR cursor
      | "write" -> WRITE cursor
      | "alloc" -> ALLOC cursor
      | "fn" -> FN cursor
      | "let" -> LET cursor
      | "rec" -> REC cursor
      | "in" -> IN cursor
      | "end" -> END cursor
      | "if" -> IF cursor
      | "then" -> THEN cursor
      | "else" -> ELSE cursor
      | "not" -> NOT cursor
      | "and" -> AND cursor
      | "or" -> OR cursor
      | "xor" -> XOR cursor
      | "fst" -> FST cursor
      | "snd" -> SND cursor
      | _ -> VAR (id, cursor) }
  | ";" { SEMICOLON (pos lexbuf) }
  | ":=" { COLONEQ (pos lexbuf) }
  | "->" { RIGHTARROW (pos lexbuf) }
  | "=" { EQ (pos lexbuf) }
  | "," { COMMA (pos lexbuf) }
  | "+" { PLUS (pos lexbuf) }
  | "-" { MINUS (pos lexbuf) }
  | "*" { MULT (pos lexbuf) }
  | "/" { DIV (pos lexbuf) }
  | "!=" { NEQ (pos lexbuf) }
  | "(" { LEFTPAREN (pos lexbuf) }
  | ")" { RIGHTPAREN (pos lexbuf) }
  | "!" { BANG (pos lexbuf) }
  | eof { EOF (pos lexbuf) }
  | "(*"
      { incr comment_depth;
        comment lexbuf;
        token lexbuf }
  | _
      { let invalid_token = Lexing.lexeme lexbuf in
        let cursor = pos lexbuf in
        let msg = F.asprintf "%a: error: invalid token: %s" Util.pp_pos cursor invalid_token in
        raise (LexingError msg) }
and comment = parse
  | "(*" { incr comment_depth; comment lexbuf }
  | "*)" { decr comment_depth; if !comment_depth > 0 then comment lexbuf }
  | eof
      { let cursor = pos lexbuf in
        let msg = F.asprintf "%a: error: invalid comment" Util.pp_pos cursor in
        raise (LexingError msg) }
  | _ { comment lexbuf }

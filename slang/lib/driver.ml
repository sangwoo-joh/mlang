module F = Format
open Config

let parse opt file =
  if opt.debug then F.fprintf F.std_formatter "Start to parsing file %s@." file ;
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  match Parser.parse Lexer.token lexbuf with
  | exp ->
      if opt.debug then F.fprintf F.std_formatter "Parsing complete@." ;
      exp
  | exception Lexer.LexingError msg ->
      F.fprintf F.err_formatter "Lexing error: %s@." msg ;
      exit 2
  | exception _ ->
      F.fprintf F.err_formatter "Syntax error@." ;
      exit 2


let run opt file =
  if opt.debug then F.fprintf F.std_formatter "Start to running file %s@." file ;
  let exp = parse opt file in
  if opt.debug && opt.level = Verbose then F.fprintf F.std_formatter "@[<hov 2>%a@]@." Exp.pp exp ;
  Interpret.eval exp ;
  if opt.debug then F.fprintf F.std_formatter "Running complete@."


let typecheck opt file =
  if opt.debug then F.fprintf F.std_formatter "Start to type check file %s@." file ;
  ( match Type_checker.check (parse opt file) with
  | exception Typ.TypeError _ ->
      F.fprintf F.std_formatter "Type Error@."
  | t ->
      F.fprintf F.std_formatter "@[<hov 2>%a@]@." Typ.pp t ) ;
  if opt.debug then F.fprintf F.std_formatter "Type checking complete@."

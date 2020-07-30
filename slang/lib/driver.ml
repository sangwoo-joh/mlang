module F = Format
open Config

let parse level file =
  let verbose = is_verbose level in
  if verbose then F.fprintf F.std_formatter "=== Start to parsing file %s@." file ;
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  match Parser.parse Lexer.token lexbuf with
  | exp ->
      if verbose then F.fprintf F.std_formatter "=== Complete parsing@." ;
      exp
  | exception Lexer.LexingError msg ->
      F.fprintf F.err_formatter "Lexing error: %s@." msg ;
      exit 2
  | exception _ ->
      F.fprintf F.err_formatter "Syntax error@." ;
      exit 2


let run level file =
  let verbose = is_verbose level in
  if verbose then F.fprintf F.std_formatter "Start to running file %s@." file ;
  let exp = parse level file in
  if verbose then F.fprintf F.std_formatter "@[<hov 2>%a@]@." Exp.pp exp ;
  ignore (Interpret.run ~verbose exp) ;
  if is_verbose level then F.fprintf F.std_formatter "Running complete@."


let typecheck level file =
  let verbose = is_verbose level in
  if verbose then F.fprintf F.std_formatter "Start to type check file %s@." file ;
  ( match Type_checker.check (parse level file) with
  | exception Typ.TypeError _ ->
      F.fprintf F.std_formatter "Type Error@."
  | t ->
      F.fprintf F.std_formatter "@[<hov 2>%a@]@." Typ.pp t ) ;
  if verbose then F.fprintf F.std_formatter "Type checking complete@."

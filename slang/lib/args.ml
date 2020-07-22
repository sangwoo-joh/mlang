open Cmdliner
module F = Format

type level = Normal | Quiet | Verbose

let debug = ref false

let level = ref Normal

let common_opt d l =
  debug := d ;
  level := l


let common_opt_t =
  let docs = Manpage.s_common_options in
  let debug =
    let doc = "Debug mode" in
    Arg.(value & flag & info ["debug"] ~docs ~doc)
  in
  let level =
    let doc = "Suppress all stdout" in
    let quiet = (Quiet, Arg.info ["q"; "quiet"] ~docs ~doc) in
    let doc = "Verbose mode" in
    let verbose = (Verbose, Arg.info ["v"; "verbose"] ~docs ~doc) in
    Arg.(last & vflag_all [Normal] [quiet; verbose])
  in
  Term.(const common_opt $ debug $ level)


let common_help_secs =
  [ `S Manpage.s_common_options
  ; `P "These options are common to all commands."
  ; `S Manpage.s_bugs
  ; `P "Please report bug at https://github.com/sangwoo-joh/slang/issues" ]


let parse file =
  if !debug then F.fprintf F.std_formatter "Start to parsing file %s@." file ;
  let chan = open_in file in
  let lexbuf = Lexing.from_channel chan in
  let exp = Parser.program Lexer.start lexbuf in
  if !debug then F.fprintf F.std_formatter "Parsing complete@." ;
  exp


let run _ file =
  if !debug then F.fprintf F.std_formatter "Start to running file %s@." file ;
  let exp = parse file in
  if !debug && !level = Verbose then Pp.M_Printer.print_exp exp ;
  M.run exp ;
  if !debug then F.fprintf F.std_formatter "Running complete@."


let file_const =
  let doc = "M language file $(docv) to execute." in
  Arg.(value & pos 0 string "" & info [] ~docv:"source" ~doc)


let run_cmd =
  let doc = "Run the M language source file." in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Run a program written in M programming language"
    ; `Blocks common_help_secs ]
  in
  (Term.(const run $ common_opt_t $ file_const), Term.info "run" ~doc ~sdocs:Manpage.s_common_options ~exits ~man)


let typecheck _ file =
  if !debug then F.fprintf F.std_formatter "Start to type check file %s@." file ;
  ( match Type_checker.check (parse file) with
  | exception M.TypeError _ ->
      print_endline "Type Error"
  | t ->
      Pp.M_Printer.print_typ t ) ;
  if !debug then F.fprintf F.std_formatter "Type checking complete@."


let typecheck_cmd =
  let doc = "Type check the M language source file." in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Type check a program written in M programming language"
    ; `Blocks common_help_secs ]
  in
  ( Term.(const typecheck $ common_opt_t $ file_const)
  , Term.info "type" ~doc ~sdocs:Manpage.s_common_options ~exits ~man )


let default_cmd =
  ( Term.(ret (const (fun _ -> `Help (`Pager, None)) $ common_opt_t))
  , Term.info "toplevel" ~version:"v0.1" ~doc:"toplevel for M language"
      ~sdocs:Manpage.s_common_options ~exits:Term.default_exits ~man:common_help_secs )


let cmds = [run_cmd; typecheck_cmd]

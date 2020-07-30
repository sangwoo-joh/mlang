open Cmdliner
open Slang

let driver () = Term.(exit @@ eval_choice Args.repl_cmd Args.cmds)

let () = driver ()

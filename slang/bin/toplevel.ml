(** SNU 4190.310 Programming Languages *)

open Cmdliner
open Slang

let driver () =
  Term.(exit @@ eval_choice Args.default_cmd Args.cmds)


let () = driver ()

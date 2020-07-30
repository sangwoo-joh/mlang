type level = Normal | Quiet | Verbose

let is_verbose = function Verbose -> true | _ -> false

let is_quiet = function Quiet -> true | _ -> false

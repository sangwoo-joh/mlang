(* SKI combinators *)

let I = fn x -> x in
let K = fn x -> fn y -> x in
let S = fn x -> (fn y -> (fn z -> (x z) (y z))) in
  S (K (S I)) (S (K K) I) 1 (fn x -> x + 1)
end
end
end
(* Result : int *)

(* SKI combinators (type error) *)

let I = fn x -> x in
let K = fn x -> fn y -> x in
let S = fn x -> fn y -> fn z -> (x z) (y z) in
  S (K (S I)) (S (K K) I) 1 (fn x -> x or 1)
end
end
end
(* Result : type error *)

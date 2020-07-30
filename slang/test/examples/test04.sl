(* Polymorphic toy 2 (type error) *)

let I = fn x -> x in
let const = fn n -> 10 in
  (I 1, I true);
  fst (1, true) + fst ("wrong", "2015 FALL 4190.310") -
  const 1 + const true + const "programming language"
end
end
(* Result : type error *)

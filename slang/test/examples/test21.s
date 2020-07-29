(* Polymorphism trap (type scheme generalization) : type check success *)

let k = fn x ->
  (* This y should not be parameterized, since x is in type env *)
  let y = x in
    (y false, y true)
  end
in
  k (fn x -> x or true)
end

(* Result : (bool, bool) *)

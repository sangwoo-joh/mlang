(* Polymorphism trap (alloc) : type check success *)
let rec f = fn x -> alloc (fn x -> x) in (* This does not invoke alloc *)
  write ((!(f 10)) true); write ((!(f true)) "hello")
end


  (* Result : string *)

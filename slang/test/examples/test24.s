(* Polymorphism trap (alloc) : type check fail *)

let my_alloc = fn x -> alloc x in
  let f = (write "calling my_alloc"; my_alloc (fn x -> x)) in
    f := (fn x -> x + 1); write ((!f) 10); (!f) true
  end
end

(* Result : type error *)

(* Polymorphism with WRITE + EQUAL (type error)*)

let bar = fn x -> fn y ->
  if x = y then
    write x
  else
    write y
in
  let i = 1 in
  let s = "hello world" in
  let b = true in
  let l = alloc 10 in
    (bar i 2, bar s "bye world");
    (fn z -> (z, bar (alloc "aa") (alloc "bb"))) (bar b false)
  end
  end
  end
  end
end

(* Result : type erro *)

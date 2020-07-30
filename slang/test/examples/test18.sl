(* Polymorphism with EQUAL (type error) *)

let foo = fn x -> fn y ->
  if x = y then x else y
in
  let i = 1 in
  let s = "hello world" in
  let b = true in
  let l = alloc 10 in
    (
      (foo i 2, foo s "bye world"),
      (foo (fn x -> x + 1) (fn y -> y - 1), foo l (alloc 20))
    )
  end
  end
  end
  end
end

(* Reseult : type error *)

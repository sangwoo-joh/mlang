(* Polymorphism with EQUAL *)

let foo = fn x -> fn y ->
  if x = y then x else y
in
  let i = 1 in
  let s = "hello world" in
  let b = true in
  let l = malloc 10 in
    (
      (foo i 2, foo s "bye world"),
      (foo b false, foo l (malloc 20))
    )
  end
  end
  end
  end
end

(* Reseult : ((int, string), (bool, loc (int))) *)

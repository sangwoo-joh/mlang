let foo = fn x -> fn y ->
  if x = y then x else y
in
  let i = 1 in
  let s = "hello world" in
  let b = true in
  let l = alloc 10 in
    (
      (foo i 2, foo s "bye world"),
      (foo b false, foo (5,1) (1,2))
    )
  end
  end
  end
  end
end
(* Result : Type checking fail -> pair compare *)

let f = fn x -> fn y ->
  write (x = y)
in
(f (alloc 1)) (alloc true)
end
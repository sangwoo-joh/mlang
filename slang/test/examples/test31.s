let rec f = fn x ->
  (x = x; alloc x; write x; x)
in
  let x = alloc 1 in f x end
end

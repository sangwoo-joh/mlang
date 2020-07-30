let rec f = fn x ->
  (f (alloc x); f (write "1"); x)
in
  ((f 1) = (f f 1), f f "1")
end

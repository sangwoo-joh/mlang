let f = fn x -> (x, (write x)) in
  alloc (alloc 10);
  ((f 1), (f true))
end

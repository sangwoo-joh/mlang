(* Polymorphic swap (type error) *)

let swap = (fn order_pair ->
  if (fst order_pair) (snd order_pair) then
    (snd order_pair)
  else
    (snd snd order_pair, fst snd order_pair))
in
  ( swap (fn pair -> fst pair + 1 = snd pair, (1, 2)),
    swap (fn pair -> fst pair or snd pair, (4, 3))
  )
end

(* Result : type error *)

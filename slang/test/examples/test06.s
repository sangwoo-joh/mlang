(* Polymorphic toy with imperatives (type error) *)

let f = fn x -> malloc x in
  let a = f 10 in
  let b = f "pl" in
  let c = f true in
    a := !a + 1;
    (c := !c + 1; b := "type checker")
  end
  end
  end
end

(* Result : type error *)

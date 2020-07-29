(* Polymorphic toy with imperatives *)


let f = fn x -> malloc x in
  let a = f 10 in
  let b = f "pl" in
  let c = f true in
    a := !a + 1;
    (b := "type checker", c := !c or false)
  end
  end
  end
end

(* Result : (string, bool) *)

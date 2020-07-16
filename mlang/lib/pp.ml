(** SNU 4190.310 Programming Languages *)

open M

module M_Printer : sig
  val print_exp : exp -> unit

  val print_typ : typ -> unit
end = struct
  let ps = print_string

  let nl = print_newline

  let indent i =
    let rec it = function
      | 0 ->
          ()
      | n ->
          ps " " ;
          it (n - 1)
    in
    nl () ;
    it i


  let rec pp n = function
    | Const (Str s) ->
        ps s
    | Const (Nat m) ->
        print_int m
    | Const (Bool true) ->
        ps "true"
    | Const (Bool false) ->
        ps "false"
    | Var s ->
        ps s
    | Fn (x, e) -> (
        ps ("fn " ^ x ^ " -> ") ;
        match e with
        | Fn _ ->
            pp (n + 1) e
        | _ ->
            indent (n + 1) ;
            pp (n + 1) e )
    | App (e, e') ->
        pp n e ;
        ps " " ;
        pp n e'
    | If (e1, e2, e3) ->
        ps "if " ;
        pp n e1 ;
        ps " then " ;
        indent (n + 1) ;
        pp (n + 1) e2 ;
        indent n ;
        ps "else" ;
        indent (n + 1) ;
        pp (n + 1) e3
    | Read ->
        ps "read "
    | Write e ->
        ps "write(" ;
        pp n e ;
        ps ")"
    | Let (d, e) ->
        let rec sugaring l acc =
          match l with
          | Let (d, Let (d', e)) ->
              sugaring (Let (d', e)) (d :: acc)
          | Let (d, e) ->
              (List.rev (d :: acc), e)
          | _ ->
              raise (Invalid_argument "impossible")
        in
        let decls, body = sugaring (Let (d, e)) [] in
        ps "let " ;
        List.iter
          (fun x ->
            indent (n + 1) ;
            printDecl (n + 1) x)
          decls ;
        indent n ;
        ps "in" ;
        indent (n + 1) ;
        pp (n + 1) body ;
        indent n ;
        ps "end"
    | Malloc e ->
        ps "malloc " ;
        pp (n + 1) e
    | Assign (e, e') ->
        pp n e ;
        ps " := " ;
        pp n e'
    | Bang e ->
        ps "!" ;
        pp n e
    | Seq (e, e') ->
        pp n e ;
        ps ";" ;
        indent n ;
        pp n e'
    | Pair (e1, e2) ->
        ps "(" ;
        pp n e1 ;
        ps ", " ;
        pp n e2 ;
        ps ")"
    | Fst e ->
        pp n e ;
        ps ".1"
    | Snd e ->
        pp n e ;
        ps ".2"
    | Bop (op, e1, e2) ->
        let op_str =
          match op with Add -> "+" | Sub -> "-" | Eq -> "=" | And -> "and" | Or -> "or"
        in
        ps "(" ;
        pp n e1 ;
        ps (" " ^ op_str ^ " ") ;
        pp n e2 ;
        ps ")"


  and printDecl n = function
    | Val (x, e) ->
        ps "val " ;
        ps (x ^ " = ") ;
        pp (n + 1) e
    | Rec (f, x, e) ->
        ps ("rec " ^ f ^ "(" ^ x ^ ") = ") ;
        pp (n + 1) e


  let rec pp_type ty =
    match ty with
    | TInt ->
        ps "int"
    | TBool ->
        ps "bool"
    | TString ->
        ps "string"
    | TPair (tau1, tau2) ->
        ps "(" ;
        pp_type tau1 ;
        ps ", " ;
        pp_type tau2 ;
        ps ")"
    | TLoc tau1 ->
        ps "loc (" ;
        pp_type tau1 ;
        ps ")"


  let print_exp = pp 0

  let print_typ ty =
    pp_type ty ;
    nl ()
end

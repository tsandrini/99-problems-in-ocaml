(* 10. Run-length encoding of a list. (easy) *)

let encode lst =
  let rec aux count acc = function
    | [] -> []
    | [x] -> (count + 1, x) :: acc
    | x :: (y :: _ as tl) -> if x = y then aux (count + 1) acc tl else
        aux 0 ((count + 1, x) :: acc) tl  in
  aux 0 [] lst |> List.rev


(* uhh, not really feeling writing the imperative version of this function  *)

let%test "encode - normal input" = encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]

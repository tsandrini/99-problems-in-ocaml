(* 11. Modified run-length encoding. (easy) *)

type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode lst =
  let toRle elem = function
    | 1 -> One elem
    | _ as c -> Many (c, elem) in
  let rec aux count acc = function
    | [] -> []
    | [x] -> (toRle x (count + 1)) :: acc
    | x :: (y :: _ as tl) -> if x = y then aux (count + 1) acc tl else
        aux 0 ((toRle x (count + 1)) :: acc) tl in
  aux 0 [] lst |> List.rev


let%test "encode - normal input" = encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
                                                                                                                    Many (4, "e")]

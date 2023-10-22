(* 5. Reverse a list. (easy) *)
(* slow, O(n^2) time complexity *)
let rec rev = function
  | [] -> []
  | hd :: tl -> rev tl @ [hd]

(* fast, linear time complexity *)
let rev' lst =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux (hd :: acc) tl
  in
  aux [] lst

(* equivalent to the second one *)
let rev'' lst =
  List.fold_left (fun acc x -> x :: acc) [] lst

(* pointfree style *)
let rev_pf'' = List.fold_left (fun acc x -> x :: acc) []

let%test "rev - normal input" = rev ["a"; "b"; "c"] = ["c"; "b"; "a"]
let%test "rev' - normal input" = rev' ["a"; "b"; "c"] = ["c"; "b"; "a"]
let%test "rev'' - normal input" = rev'' ["a"; "b"; "c"] = ["c"; "b"; "a"]
let%test "rev_pf'' - normal input" = rev_pf'' ["a"; "b"; "c"] = ["c"; "b"; "a"]

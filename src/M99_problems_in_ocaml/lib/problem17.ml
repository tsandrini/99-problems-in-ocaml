(* 17. Split a list into two parts; the length of the first part is given. (easy) *)
(* If the length of the first part is longer than the entire list, then *)
(*  the first part is the list and the second part is empty. *)

let map_tuple f (a, b) = (f a, f b)

let split lst idx =
  let rec aux (acc1, acc2) count = function
    | [] -> (acc1, acc2)
    | hd :: tl when count < idx -> aux (hd :: acc1, acc2) (count + 1) tl
    | hd :: tl -> aux (acc1, hd :: acc2) (count + 1) tl in
  aux ([], []) 0 lst |> map_tuple List.rev


let%test "split - normal input" = split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3 = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
let%test "split - overflow input" = split ["a"; "b"; "c"; "d"] 5 = (["a"; "b"; "c"; "d"], [])

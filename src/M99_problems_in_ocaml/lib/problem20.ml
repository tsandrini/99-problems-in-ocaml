(* 20. Remove the K'th element from a list. (easy) *)

let remove_at n lst =
  let rec aux acc idx = function
    | [] -> acc
    | _ :: tl when idx = n -> aux acc (idx + 1) tl
    | hd :: tl -> aux (hd :: acc) (idx + 1) tl in
  aux [] 0 lst |> List.rev

let%test "remove_at - normal input" = remove_at 1 ["a"; "b"; "c"; "d"] = ["a"; "c"; "d"]

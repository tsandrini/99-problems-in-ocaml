(* 18. Extract a slice from a list. (medium) *)

(* Given two indices, i and k, the slice is the list containing the elements *)
(*  between the i'th and k'th element of the original list (both limits *)
(*   included). Start counting the elements with 0 (this is the way the *)
(*    List module numbers elements). *)

let slice lst start_idx end_idx =
  let rec aux acc count = function
    | [] -> acc
    | hd :: tl when count >= start_idx && count <= end_idx -> aux (hd :: acc) (count + 1) tl
    | _ :: tl -> aux acc (count + 1) tl in
  aux [] 0 lst |> List.rev

let%test "slice - normal input" = slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6 = ["c"; "d"; "e"; "f"; "g"]

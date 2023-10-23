(* 16. Drop every N'th element from a list. (medium) *)

let drop lst num =
  let rec aux acc counter = function
    | [] -> acc
    | hd :: tl when counter <> 1 -> aux (hd :: acc) (counter - 1) tl
    | _ :: tl -> aux acc num tl in
  aux [] num lst |> List.rev


let%test "drop - normal input" = drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3 = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]

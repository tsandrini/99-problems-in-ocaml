(* 9. Pack consecutive duplicates of list elements into sublists. (medium) *)

let pack lst =
  let rec aux current acc = function
    | [] -> []
    | [x] -> (x :: current) :: acc
    | x :: (y :: _ as tl) -> if x = y then aux (x :: current) acc tl else
        aux [] ((x :: current) :: acc) tl in
  aux [] [] lst |> List.rev

let pack' lst =
  let n = List.length lst in
  if n = 0 then [] else
    let arr = Array.of_list lst in
    let result = ref [||] in
    let current_group = ref [Array.get arr 0] in
    let i = ref 1 in
    while !i < n do
      if Array.get arr !i = List.hd !current_group then
        current_group := Array.get arr !i :: !current_group
      else
        begin
          let new_group = Array.of_list (List.rev !current_group) in
          let new_result = Array.append !result [|new_group|] in
          result := new_result;
          current_group := [Array.get arr !i];
        end;
      i := !i + 1;
    done;
    let final_group = Array.of_list (List.rev !current_group) in
    let final_result = Array.append !result [|final_group|] in
    Array.to_list (Array.map Array.to_list final_result)


let%test "pack - normal input" = pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"] = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
                                                                                                                     ["e"; "e"; "e"; "e"]]
let%test "pack' - normal input" = pack' ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"] = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
                                                                                                                       ["e"; "e"; "e"; "e"]]

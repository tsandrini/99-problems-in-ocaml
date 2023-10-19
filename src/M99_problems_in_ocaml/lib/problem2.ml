(* 2. Find the last but one (last and penultimate) elements of a list. (easy) *)


let%test "" = last_two ["a"; "b"; "c"; "d"] = Some ("c", "d")
let%test "" = last_two ["a"] = None
let%test "" = last_two [] = None

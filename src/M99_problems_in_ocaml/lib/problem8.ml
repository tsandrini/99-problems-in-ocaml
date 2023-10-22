(* 8. Eliminate consecutive duplicates of list elements. (medium) *)

let rec compress = function
  | [] -> []
  | [x] -> [x]
  | fst :: snd :: tl -> if fst = snd then compress (snd :: tl) else fst :: compress (snd :: tl)

let rec compress' = function
  | [] | [_] as l -> l
  | fst :: ( snd :: _ as tl  ) when fst = snd -> compress' tl
  | fst :: tl -> fst :: compress' tl

let compress'' lst =
  let n = List.length lst in
  if n = 0 then [] else
    let out = Array.make n (List.hd lst) in
    let j = ref 1 in
    for i = 1 to n - 1 do
      let prev = out.(!j - 1) in
      let curr = List.nth lst i in
      if prev <> curr then begin
        out.(!j) <- curr;
        incr j
      end
    done;
    Array.to_list (Array.sub out 0 !j)


let%test "compress - normal input" =  compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = ["a"; "b"; "c"; "a"; "d"; "e"]
let%test "compress' - normal input" =  compress' ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = ["a"; "b"; "c"; "a"; "d"; "e"]
let%test "compress'' - normal input" =  compress'' ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = ["a"; "b"; "c"; "a"; "d"; "e"]

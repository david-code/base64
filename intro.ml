open StdLabels

let byte_triplets bytes =
  let remainder = (Array.length bytes) mod 3 in
  let m = Array.length bytes / 3 in
  let n = if remainder > 0 then m + 1 else m in
  Array.init n ~f:(fun i -> if i < m
                            then Array.sub bytes (i * 3) 3
                            else Array.sub bytes (i * 3) remainder)


let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
let bytes c =
  match c with
  | [|  |] -> [| |]
  | [| c |] -> [| Char.code c |]
  | [| c1 ; c2 |] -> [| Char.code c1 ; Char.code c2|]
  | [| c1 ; c2 ; c3 |] -> [| Char.code c1 ; Char.code c2 ; Char.code c3 |]
  | _ -> failwith "Bad triplet size"
let bytes_to_int32 b =
  if Array.length b > 4 then failwith "overflow" else ();
  let f = (fun x y -> y lor (x lsl 8)) in
  let init = 0 in
  Array.fold_left ~f ~init b
let sextets_to_chars s =
  Array.map ~f:(String.get chars) s

let num_sextets x =
  let rec f x acc =
    if x = 0 then acc else f (x lsr 6) (1 + acc)
  in
  f x 0
let int32_to_sextets i32 =
  let mask = 0b111111 lsl (18) in
  let n = num_sextets i32 in
  Array.init n ~f:(fun i -> (mask land (i32 lsl (6 * i))) lsr 18)

let decode_bytes b =
  b |> bytes |> bytes_to_int32 |> int32_to_sextets |> sextets_to_chars

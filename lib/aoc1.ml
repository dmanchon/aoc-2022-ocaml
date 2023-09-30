let solve1 input =
  let sum s = String.split_on_char '\n' (String.trim s)
              |> List.map int_of_string
              |> List.fold_left (+) 0 in
  Str.split (Str.regexp "\n\n") input
  |> List.map sum
  |> List.sort (-)
  |> List.rev
  |> List.hd;;

let solve2 input =
  let sum s = String.split_on_char '\n' (String.trim s)
              |> List.map int_of_string
              |> List.fold_left (+) 0 in
  let groups = Str.split (Str.regexp "\n\n") input
               |> List.map sum
               |> List.sort (-)
               |> List.rev in
  match groups with
  | g1 :: g2 :: g3 :: _ -> g1 + g2 + g3
  | _ -> failwith "not enough groups";;
  


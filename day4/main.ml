open Core;;

let fully_contained ((a1, a2), (b1, b2)) =
  a1 <= b1 && a2 >= b2 || a1 >= b1 && a2 <= b2;;


let overlaps ((a1, a2), (b1, b2)) =
  (a1 <= b1 && b1 <= a2)
  || (a1 <= b2 && b2 <= a2)
  || (b1 <= a1 && a1 <= b2)
  || (b1 <= a2 && a2 <= b2);;

let parse_interval s =
  match String.split_on_chars s ~on:['-';','] |> List.map ~f:int_of_string with 
  | x::y::z::t::[] -> ((x, y), (z, t))
  | _ -> failwith "bad input";;

let solve1 input =
  In_channel.read_lines input 
  |> List.map  ~f:parse_interval
  |> List.filter  ~f:fully_contained 
  |> List.length;;

let solve2 input =
  In_channel.read_lines input 
  |> List.map  ~f:parse_interval
  |> List.filter  ~f:overlaps 
  |> List.length;;
    
Printf.printf "Part1: %d\nPart2: %d\n" (solve1 "day4/input.txt") (solve2 "day4/input.txt");;
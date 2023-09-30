
let _ = "A Y
B X
C Z";;

let slurp file =
  let ch = open_in file in
  let len = in_channel_length ch in
  let s = really_input_string ch len in
  close_in_noerr ch;
  s;;

let play1 = function
  | "A X" -> 1 + 3
  | "A Y" -> 2 + 6
  | "A Z" -> 3 + 0
  | "B X" -> 1 + 0
  | "B Y" -> 2 + 3
  | "B Z" -> 3 + 6
  | "C X" -> 1 + 6
  | "C Y" -> 2 + 0
  | "C Z" -> 3 + 3
  | _ -> 0;;


let play2 = function
  | "A X" -> 3 + 0
  | "A Y" -> 1 + 3
  | "A Z" -> 2 + 6
  | "B X" -> 1 + 0
  | "B Y" -> 2 + 3
  | "B Z" -> 3 + 6
  | "C X" -> 2 + 0
  | "C Y" -> 3 + 3
  | "C Z" -> 1 + 6
  | _ -> 0;;



let solve1 = String.split_on_char '\n' (slurp "day2/input.txt")
|> List.map play1
|> List.fold_left (+) 0 ;;

let solve2 = String.split_on_char '\n' (slurp "day2/input.txt")
|> List.map play2
|> List.fold_left (+) 0 ;;


Printf.printf "Part1: %d\nPart2: %d\n" solve1 solve2

open Aoc1;;

let file = "day1/inputs/day1.txt"

let input =
  let ch = open_in file in
  let len = in_channel_length ch in
  really_input_string ch len;;


Printf.printf "Part1: %d\nPart2: %d\n" (solve1 input) (solve2 input) 

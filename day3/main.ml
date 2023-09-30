let input1 = "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw";;


let slurp file =
  let ch = open_in file in
  let len = in_channel_length ch in
  let s = really_input_string ch len in
  close_in_noerr ch;
  s;;

module CharSet = Set.Make(Char);;

let string_to_char_list s =
  s |> String.to_seq |> List.of_seq;;

let set_of_string s = 
  let rec helper set lst = 
    match lst with
    | [] -> set
    | h :: t -> helper (CharSet.add h set) t in 
  helper CharSet.empty (string_to_char_list s);;

let scores = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";;

let solve1 input =
  let process_line line = 
    let len = (String.length line) / 2 in
    let s1 = set_of_string (String.sub line 0 len) in
    let s2 = set_of_string (String.sub line len len) in
    let chr = CharSet.inter s1 s2 
              |> CharSet.to_list |> List.hd in 
    String.index scores chr + 1 in
  input |> String.split_on_char '\n' |> List.map process_line |> List.fold_left ( + ) 0;;

let solve2 input =
  let process_group x y z = 
    let s1 = set_of_string z in
    let s2 = set_of_string y in 
    let s3 = set_of_string x in
    let chr = CharSet.inter (CharSet.inter s1 s2) s3 |> CharSet.to_list |> List.hd in
    String.index scores chr + 1 in
  let rec loop lst acc = match lst with
    | [] -> acc
    | x :: y :: z :: t -> loop t ((process_group x y z) :: acc) 
    | _ -> failwith "list lenght is not multiple of 3" in
  loop (String.split_on_char '\n' input) [] 
  |> List.fold_left (+) 0;;

Printf.printf "Part1: %d\nPart2: %d\n" (solve1 (slurp "day3/input.txt")) (solve2 (slurp "day3/input.txt"));;
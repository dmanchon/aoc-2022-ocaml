open Aoc1;;

let input = "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000";;


let () =
  let answer = solve1 input in
  assert (answer == 24000);;

let () =
  let answer = solve2 input in
  assert (answer == 45000);;

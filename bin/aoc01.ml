(* https://adventofcode.com/2024/day/1 *)

let parse_line l =
  let list_split = Str.split (Str.regexp "   ") l in
  let left_num = List.nth list_split 0 in
  let right_num = List.nth list_split 1 in
  (int_of_string left_num, int_of_string right_num)
;;

(* Answer: 2000468 *)
let part1 lines =
  let parsed = List.map parse_line lines in
  let (left, right) = Jls.unzip parsed in
  let left_sorted = List.sort Int.compare left in
  let right_sorted = List.sort Int.compare right in
  let zipped = Jls.zip left_sorted right_sorted in
  let diff acc (v1, v2) = acc + abs (v1 - v2) in

  List.fold_left diff 0 zipped
;;

(* Answer: 18567089 *)
let part2 lines =
  let parsed = List.map parse_line lines in
  let (left, right) = Jls.unzip parsed in
  let count l n =
    List.fold_left (fun acc v -> acc + (if v == n then 1 else 0)) 0 l
  in
  List.fold_left (fun acc v -> acc + v * (count right v)) 0 left
;;


let () =
  let lines = Core.In_channel.read_lines "inputs/01.txt" in
  let part1_soln = part1 lines in
  let part2_soln = part2 lines in
  Printf.printf "Part 1: %i\n" part1_soln;
  Printf.printf "Part 2: %i\n" part2_soln;

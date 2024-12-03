(* https://adventofcode.com/2024/day/2 *)

let parse_line l =
  l
  |> Str.split (Str.regexp " ")
  |> List.map int_of_string
;;

let rec adj_close_enough' ls v1 v2 =
  let diff = abs (v1 - v2) in
  let is_valid = (diff <= 3) && (diff >= 1) in

  match ls with
  | h1::tl -> if is_valid
    then adj_close_enough' tl v2 h1
    else false
  | _ -> is_valid
;;

let adj_close_enough ls =
  match ls with
  | h1::h2::tl -> adj_close_enough' tl h1 h2
  | _ -> true
;;

let is_safe levels =
  let all_inc ls = List.equal (==) ls (List.sort_uniq Int.compare ls) in
  let all_dec ls = List.equal (==) ls (List.sort_uniq Int.compare ls |> List.rev) in

  ((all_inc levels) || (all_dec levels)) && (adj_close_enough levels)
;;

(* Answer: 383 *)
let part1 lines =
  let parsed = List.map parse_line lines in
  let count_safe acc l = if (is_safe l) then acc + 1 else acc in

  List.fold_left count_safe 0 parsed
;;

let rec one_removed prev curr rem acc =
  match rem with
  | [] -> prev::acc
  | hd::tl -> one_removed (prev @ [curr]) hd tl ((prev @ rem)::acc)
;;

let one_removed l =
  match l with
  | hd::tl -> one_removed [] hd tl []
  | _ -> [l]
;;

(* Answer: 436 *)
let part2 lines =
  let parsed = List.map parse_line lines in
  let is_tolerant_safe line = 
    let any_safe acc l = acc || is_safe l in
    let removed = one_removed line in
    List.fold_left any_safe false removed
  in
  let count_safe acc l = if (is_tolerant_safe l) then acc + 1 else acc in

  List.fold_left count_safe 0 parsed
;;


let () =
  let lines = Jls.read_lines "inputs/02.txt" in
  (* let lines = Jls.read_lines "inputs/02_small.txt" in *)
  let part1_soln = part1 lines in
  let part2_soln = part2 lines in
  Printf.printf "Part 1: %i\n" part1_soln;
  Printf.printf "Part 2: %i\n" part2_soln;

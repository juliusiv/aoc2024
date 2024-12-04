(* https://adventofcode.com/2024/day/3 *)

let mul_re = Re.compile Re.(seq [str "mul("; rep1 digit ; str "," ; rep1 digit; str ")" ]);;
let mul_do_dont_re = Re.compile Re.(
  alt [
    seq [str "mul("; rep1 digit ; str "," ; rep1 digit; str ")" ];
    str "do()";
    str "don't()";
  ]
);;
let num_re = Re.compile Re.(rep1 digit);;

(* Answer: 174103751 *)
let part1 content =
  let extract_nums mul = Re.matches num_re mul |> List.map int_of_string in

  let matches = Re.matches mul_re content in
  let pairs = List.map extract_nums matches in
  let mult pair = List.fold_left (fun acc n -> acc * n) 1 pair in

  List.fold_left (fun acc p -> acc + mult p) 0 pairs
;;

(* Answer: 100411201 *)
let part2 content =
  let extract_nums mul = Re.matches num_re mul |> List.map int_of_string in

  let matches = Re.matches mul_do_dont_re content in
  let rec remove_mults removing acc ms =
    match ms with
    | [] -> acc
    | hd::tl ->
      match hd, removing with
      | "do()", _ -> remove_mults true acc tl
      | "don't()", _ -> remove_mults false acc tl
      | _, false -> remove_mults removing acc tl
      | mult, true -> remove_mults removing (mult::acc) tl
  in
  let pairs =
    matches
    |> remove_mults true []
    |> List.map (Re.matches mul_re)
    |> List.flatten
    |> List.map extract_nums
  in
  let mult pair = List.fold_left (fun acc n -> acc * n) 1 pair in

  List.fold_left (fun acc p -> acc + mult p) 0 pairs
;;

let () =
  let content = Jls.read_file "inputs/03.txt" in
  let part1_soln = part1 content in
  let part2_soln = part2 content in
  Printf.printf "Part 1: %i\n" part1_soln;
  Printf.printf "Part 2: %i\n" part2_soln;

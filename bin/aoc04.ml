(* https://adventofcode.com/2024/day/4 *)

let search_dirs r c =
  let dirs = [
    [(r, c); (r, c+1); (r, c+2); (r, c+3)];
    [(r, c); (r, c-1); (r, c-2); (r, c-3)];
    [(r, c); (r+1, c); (r+2, c); (r+3, c)];
    [(r, c); (r-1, c); (r-2, c); (r-3, c)];
    [(r, c); (r+1, c+1); (r+2, c+2); (r+3, c+3)];
    [(r, c); (r-1, c+1); (r-2, c+2); (r-3, c+3)];
    [(r, c); (r-1, c-1); (r-2, c-2); (r-3, c-3)];
    [(r, c); (r+1, c-1); (r+2, c-2); (r+3, c-3)];
  ] in
  let max = 140 in
  let is_valid (r, c) = r >= 0 && r < max && c >= 0 && c < max in
  let has_invalid_loc locs =
    let valid_locs = List.filter is_valid locs in
    (List.length valid_locs) < 4
  in
  List.filter (fun locs -> not (has_invalid_loc locs)) dirs
;;


(* Answer: 2534 *)
let part1 lines =
  let grid = List.map (fun l -> l |> String.to_seq |> List.of_seq) lines in
  let get_words grid dirs =
    dirs
    |> List.map (fun (r, c) ->
      let w = List.nth (List.nth grid r) c in
      (* Printf.printf "char: %c" w; *)
      w
    )
    |> List.to_seq
    |> String.of_seq
  in

  let all_words = grid
    |> List.mapi (fun r row ->
      row |> List.mapi (fun c _ ->
        (* let w = List.nth (List.nth grid r) c in *)
        let words = search_dirs r c |> List.map (get_words grid) in
        (* Printf.printf "char: %c, %i, %i\n" w r c;
        Printf.printf "len: %i\n" (List.length (search_dirs r c)); *)
        (* List.iter (Printf.printf "word: %s\n") words; *)
        
        words
      )
    )
    |> List.flatten
    |> List.flatten
  in
  let xmases = all_words |> List.filter (fun w -> String.equal w "XMAS") in

  List.length xmases
;;

let xmas_dirs r c =
  let dirs = [
    [(r-1, c-1); (r, c); (r+1, c+1)]; (* \ *)
    [(r+1, c-1); (r, c); (r-1, c+1)]; (* / *)
  ] in
  let max = 140 in
  let is_valid (r, c) = r >= 0 && r < max && c >= 0 && c < max in
  let has_invalid_loc locs =
    let valid_locs = List.filter is_valid locs in
    (List.length valid_locs) < 3
  in
  let any_invalid = has_invalid_loc (List.nth dirs 0) || has_invalid_loc (List.nth dirs 1) in
  
  if any_invalid then [[(0, 0)]] else dirs
;;

(* Answer:  *)
let part2 lines =
  let grid = List.map (fun l -> l |> String.to_seq |> List.of_seq) lines in
  let get_words grid dirs =
    dirs
    |> List.map (fun (r, c) ->
      let w = List.nth (List.nth grid r) c in
      (* Printf.printf "char: %c" w; *)
      w
    )
    |> List.to_seq
    |> String.of_seq
  in

  let xmas_counts = grid
    |> List.mapi (fun r row ->
      row |> List.mapi (fun c _ ->
        (* let w = List.nth (List.nth grid r) c in *)
        let possible_xmas = xmas_dirs r c |> List.map (get_words grid) in
        let is_xmas w = String.equal w "MAS" || String.equal w "SAM" in
        (* Printf.printf "char: %c, %i, %i\n" w r c;
        Printf.printf "len: %i\n" (List.length (search_dirs r c)); *)
        (* List.iter (Printf.printf "word: %s\n") words; *)
        let both_xmas = is_xmas (List.nth possible_xmas 0) && is_xmas (List.nth possible_xmas 1) in
        
        if both_xmas then 1 else 0
      )
    )
    |> List.flatten
  in

  List.fold_left (+) 0 xmas_counts
;;


let () =
  (* let lines = Jls.read_lines "inputs/04_small.txt" in *)
  let lines = Jls.read_lines "inputs/04.txt" in
  let part1_soln = part1 lines in
  let part2_soln = part2 lines in
  Printf.printf "Part 1: %i\n" part1_soln;
  Printf.printf "Part 2: %i\n" part2_soln;

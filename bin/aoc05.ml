(* https://adventofcode.com/2024/day/5 *)

let contains list num =
  match List.find_index (Int.equal num) list with
  | Some _ -> false
  | None -> true
;;

let is_valid_update orderings updates =
  let rem_valid curr rem =
    Jls.every (fun r ->
      let ordering = Hashtbl.find_all orderings r in
      contains ordering curr
    ) rem
  in
  let rec is_valid curr rem valid =
    match rem with
    | [] -> true && valid
    | hd::tl -> is_valid hd tl (valid && rem_valid curr tl)
  in
  
  is_valid  (List.hd updates) (List.tl updates) true
;;

let get_middle_page updates =
  let len = List.length updates in

    List.nth updates (len / 2)
;;

(* Answer: 4905 *)
let part1 orderings page_updates =
  let valid_updates = List.filter (is_valid_update orderings) page_updates in
  let middle_pages = List.map get_middle_page valid_updates in

  List.fold_left (+) 0 middle_pages
;;

(* Answer: 6204 *)
let part2 orderings page_updates =
  let invalid_updates = List.filter (fun u -> is_valid_update orderings u |> not) page_updates in
  let sorter a b =
    if Int.equal a b then
      0
    else
      let ordering = Hashtbl.find_all orderings a in
      if contains ordering b then
        -1
      else
        1
  in
  let sorted_updates = List.map (List.sort sorter) invalid_updates in
  let middle_pages = List.map get_middle_page sorted_updates in

  List.fold_left (+) 0 middle_pages
;;



let () =
  let content = Jls.read_file "inputs/05.txt" in
  let split = String.split_on_char '-' content in
  let orderings_content = List.nth split 0 in
  let page_updates_content = List.nth split 1 in

  let orderings = Hashtbl.create 100 in
  String.split_on_char '\n' orderings_content
  |> List.filter (fun s -> String.length s > 0)
  |> List.map (fun l -> String.split_on_char '|' l |> List.map int_of_string)
  |> List.iter (fun o -> match o with
  | [p1; p2] -> Hashtbl.add orderings p1 p2
  | _ -> ());

  let page_updates = String.split_on_char '\n' page_updates_content
  |> List.filter (fun s -> String.length s > 0)
  |> List.map (fun l -> String.split_on_char ',' l |> List.map int_of_string) in

  let part1_soln = part1 orderings page_updates in
  Printf.printf "Part 1: %i\n" part1_soln;
  let part2_soln = part2 orderings page_updates in
  Printf.printf "Part 2: %i\n" part2_soln;

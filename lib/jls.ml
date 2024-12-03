(* The Julius Library Standard. A collection of util shit that's useful across the problems. *)

let rec unzip' l l1 l2 =
  match l with
  | [] -> (l1, l2)
  | [(t1, t2)] -> (t1::l1, t2::l2)
  | (t1, t2)::rest -> unzip' rest (t1::l1) (t2::l2)
;;

let unzip l = unzip' l [] [];;

let rec zip' l1 l2 acc =
  match (l1, l2) with
  | ([], _) -> acc
  | (_, []) -> acc
  | (h1::t1, h2::t2) -> zip' t1 t2 ((h1, h2)::acc)
;;

let zip l1 l2 = zip' l1 l2 [];;


let read_lines (file_name : string) : string list =
  let chan = open_in file_name in
  let rec read_lines (chan : in_channel) : string list =
    try
      let line = input_line chan in
      line :: read_lines chan
    with
    | End_of_file -> []
  in
  let lines = read_lines chan in
  close_in chan;
  lines
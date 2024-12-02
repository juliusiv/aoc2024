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

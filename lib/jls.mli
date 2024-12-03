(* This is like a C header file and defines the public interface of the functions, otherwise everything in jls.ml is visible *)

val unzip : ('a * 'a) list -> 'a list * 'a list

val zip : 'a list -> 'a list -> ('a * 'a) list

val read_lines : string -> string list
(* file: symbol.mli
   author: Bob Muller

   CS3366 Programming Languages

   This file contains an API for the environment module.
*)
type t

val fromString : string -> t
val toString : t -> string
val compare : t -> t -> int

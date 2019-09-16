(* file: env.mli
   author: Bob Muller

   CS3366 Programming Languages

   This file contains the API for environments for principles of
   programming languages.
*)
open Symbol

type key = Symbol.t
type 'a map

val add   : key -> 'a -> 'a map -> 'a map
val find  : key -> 'a map -> 'a
val empty : 'a map

(* makeEnv : 'a list -> 'a map
*)
val makeEnv : 'a list -> 'a map
val toString : ('a -> string) -> 'a map -> string

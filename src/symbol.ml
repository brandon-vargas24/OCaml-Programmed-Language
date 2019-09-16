(* file: symbol.ml
   author: Bob Muller

   CS3366 Programming Languages

   This file contains an implementation of symbols.
*)
type t = String.t

let fromString str = str
let toString   sym = sym

let compare = String.compare

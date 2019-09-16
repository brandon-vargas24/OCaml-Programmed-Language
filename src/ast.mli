(* file: ast.mli
   author: Bob Muller

   CS3366 Programming Languages

   This file contains the abstract syntax for Mercury.
*)
open Symbol

type t =
  | Literal of int
  | App of {rator : Symbol.t; rands : t list}

val i2i : int   -> t
val toString : t -> string

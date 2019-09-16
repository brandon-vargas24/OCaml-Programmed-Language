(* file: dynamic.mli
   author: Bob Muller

   CS3366 Programming Languages

  This file contains code relating to dynamic alues.  NB: the purpose of
  the BinOp constructor is to enable us to carry the implementations of
  primitive operators in the value environment.
*)
open Env

type t = Literal of int 
       | BinOp of (t -> (t -> t))

val toString : t -> string

val env : t Env.map
val envToString : t Env.map -> string

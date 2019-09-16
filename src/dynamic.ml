(* file: dynamic.ml
   author: Bob Muller

   CS3366 Programming Languages

   This file contains code relating to run-time values.
*)
open Util
open Symbol
open Env

module U = Util

type t =
  | Literal of int
  | BinOp of (t -> (t -> t))

let rec toString value =
  match value with
  | Literal bits -> string_of_int bits
  | BinOp _ -> "BinOp"

(**********************************************************************)

let iXi2i op =
  let theOp opnd1 opnd2 =
    match (opnd1, opnd2) with
    | (Literal bits1, Literal bits2) -> Literal (op bits1 bits2)
    | _ -> failwith "binaryPrimOp: bad operator inputs, shouldn't happen."
  in
  BinOp theOp

(* The implementations of division must check for a zero divisor at run-time.
*)
let divI (m : int) (n : int) : int =
  match n = 0 with
  | true  -> failwith "divide: attempting to divide by zero."
  | false -> m / n

(* The order here must match the order of the operator names in the Basis 
  module.
*)
let operators = [iXi2i (+); iXi2i (-); iXi2i ( * ); iXi2i divI; iXi2i (mod)]

let env = Env.makeEnv operators

let envToString = Env.toString toString

(* file: eval.mli
   author: Bob Muller

   CS3366 Programming Languages

   This file contains the API for an evaluator for the mini-PL Mercury.
*)
open Dynamic
open Env
open Ast

val eval : Dynamic.t Env.map -> Ast.t -> Dynamic.t

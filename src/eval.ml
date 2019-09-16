(* file: eval.ml
   author: Bob Muller

   CS3366 Programming Languages

   This file contains an evaluator for the mini-PL Mercury.
*)
open Ast
open Debug
open Dynamic
open Env

module L = List

(* Debugging
*)
let dbg = Debug.out "eval"
let fmt = Format.sprintf

(* eval : Dynamic.t Env.t -> Ast.t -> Dynamic.t
*)
let rec eval env ast = Dynamic.Literal 0
    match ast with
    |Ast.Literal {typ; bit} ->
      Dynamic.Bits {typ = typ; bit = bit}

    | Ast.App {rator = Ast.Var id; rands} ->
      let rands2 = List.map (eval env) rands in
      let z = Env.find id env
      in
      (match (z, rands2) with
       | (Dynamic.BinOp, [operand1; operand2]) -> op operand1 operand2
       | _ -> failwith "operation was wrong")

    | _ -> failwith "eval was wrong case"

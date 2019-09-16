(* file: repl.ml
   author: Bob Muller

   CS3366 Programming Languages

   This file contains a REPL for the mini-language Mercury.
*)
open Debug
open Token
open Tokenizer
open Ast
open Parser
open Eval
open Dynamic

(* Debugging
*)
let dbg = Debug.out "eval"
let fmt = Format.sprintf
let error = Printf.printf

let rec repl env : unit =
  let tokens = Tokenizer.tokenizer "\nMercury> "
  in
  match tokens with
  | Token.QUIT :: [] -> ()
  | _ ->
    let ast = Parser.parser tokens in
    let astString = (Ast.toString ast) in
    let value = Eval.eval env ast in
    let valueString = Dynamic.toString value
    in
    Printf.printf "ast = %s\nvalue = %s\n" astString valueString
    ; repl env

let go () = repl Dynamic.env

let s = go ()

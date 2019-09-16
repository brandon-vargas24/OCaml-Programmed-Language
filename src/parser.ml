(* file: parser.ml
  author: Bob Muller

  CS3366 Programming Languages

   This code implements a recursive descent parser for the mini-PL
   Mercury --- the simplest programming language with just integers.

  Terms:

  E ::= E + T | E - T | T
  T ::= T * F | T / F | T % F | F
  F ::= Integer | ( E )
*)

open Token
open Ast
open Debug
open symbol

let dbg = Debug.out "eval"
let fmt = Format.sprintf

let rec expression tokens =  (Ast.Literal 0, [])
  let (ast1, tokens) = part tokens
  in
  expr_end tokens ast1

and expr_end tokens ast1 =
    match tokens with
    | Token.PLUS :: tokens ->
      let (ast2, tokens) = part tokens in
      let plus = Symbol.fromString "+" in
      let ast = Ast.App {rator = Ast.Var plus; rands = [ast1; ast2]}
      in
      expr_end tokens ast

    | Token.MINUS :: tokens ->
      let (ast2, tokens) = part tokens in
      let minus = Symbol.fromStrong "-" in
      let ast = Ast.App [rator = Ast.Var minus; rands = [ast1; ast2]]
      in expr_end tokens ast

    | _ -> (ast1, tokens)

and part tokens =
  let (ast1, tokens) = factor tokens
  in
  part_end tokens ast1

and part_end tokens ast1 =
  match tokens with
  | Token.TIMES :: tokens ->
    let (ast2, tokens) = factor tokens in
    let times = Symbol.fromString "*" in
    let ast = Ast.App {rator = Ast.Var times; rands = [ast1; ast2]}
    in
    expr_end tokens ast

  | Token.DIV :: tokens ->
      let (ast2, tokens) = factor tokens in
      let div = Symbol.fromString "/" in
      let ast = Ast.App {rator = Ast.Var div; rands = [ast1; ast2]}
      in
      expr_end tokens ast

  | Token.MOD :: tokens ->
    let (ast2, tokens) = factor tokens in
    let md = Symbol.fromString "%" in
    let ast = Ast.App {rator = Ast.Var md; rands = [ast1; ast2]}
    in
    expr_end tokens ast

  | _ -> (ast1, tokens)

and factor tokens = factor tokens =
  let (t1Ast, tokens) = sifter tokens
  in
  match tokens with

  | _ -> (t1Ast, tokens)

and sifter tokens =
  match tokens with
  | (Token.INTEGER i) :: tokens  -> (Ast.i2i i, tokens)

  | Token.LPAR :: tokens ->
    let (expr1, tokens) = expression tokens
    in
    (match tokens with
     | Token.RPAR :: tokens -> (expr1, tokens)  (* ( expr ) *)
     | _ -> failwith "( expr didnt have , or )")


let parser tokens =
  dbg (fmt "tokens = %s" (Token.toStrings tokens));
  match expression tokens with
  | (ast, []) -> ast
  | _ -> failwith "oooops, fix your syntax, leftover tokens."

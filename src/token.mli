(* file: token.mli
   author: Bob Muller

   CS3366 Programming Languages

   This file contains an API for the token module.
*)
open Symbol

type t =
  | UNIT | PLUS | MINUS | TIMES | DIV
  | RPLUS | RMINUS | RTIMES | RDIV | MOD | POW
  | LPAR | RPAR | LBRACE | RBRACE | LET | IN
  | INL  | INR | FIRST | SECOND
  | EQ | NE | GT | GE | LT | LE | DOT | COMMA | SEMI
  | COLON | IF | THEN | ELSE | CASE | OF | TRUE | FALSE
  | INTEGER of int | FLOATING of float | ID of Symbol.t
  | INT_TYPE | BOOL_TYPE | REAL_TYPE | UNIT_TYPE | ARROW
  | DARROW | BAR | QUIT

val toString  : t -> string
val toStrings : t list -> string

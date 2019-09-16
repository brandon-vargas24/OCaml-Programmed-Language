(* file: ast.ml
   author: Bob Muller

   CS3366 Programming Languages

   This file contains the abstract syntax for the mini-PL Mercury, used in
  CS3366 Principles of Programming Languages.
*)
open Symbol

module L = List

let fmt = Printf.sprintf

type t =
  | Literal of int
  | App of {rator : Symbol.t; rands : t list}

(* The following function is used in the parser to convert OCaml
   literals to Mercury literals.
*)
let i2i (i : int) : t = Literal i

(* toString : t -> string
*)
let rec toString ast =
  match ast with
  | Literal bits -> string_of_int bits

  | App {rator; rands} ->
    let ras = Symbol.toString rator in
    let randss = L.map toString rands in
    let folder s t = s ^ ", " ^ t in
    let randStrings = L.fold_left folder (L.hd randss) (L.tl randss)
    in
    fmt "%s(%s)" ras randStrings

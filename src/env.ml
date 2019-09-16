(* file: env.ml
   author: Bob Muller

   CS3366 Programming Languages

   This file contains code for environments for principles of
   programming languages.
*)
open Symbol
open Basis

module M = Map.Make(Symbol)

let fmt = Printf.sprintf

type key = Symbol.t
type 'a map = 'a M.t

let add   = M.add
let find  = M.find
let empty = M.empty

(* makeEnv : 'a list -> 'a map
*)
let makeEnv values =
  let folder map (key, value) = add key value map in
  let keyValuePairs = List.combine Basis.operatorNames values
  in
  List.fold_left folder empty keyValuePairs

let toString stringer map =
  let bindings = M.bindings map in
  let folder s (key, value) =
    fmt "%s = %s; %s" (Symbol.toString key) (stringer value) s
  in
  fmt "{%s}" (List.fold_left folder "" bindings)

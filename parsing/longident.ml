type t =
  Lident of string
| Ldot of t * string
(* | Lapply of t * t *)


let lid str = Lident str
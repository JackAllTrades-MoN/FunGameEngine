type t =
  Lident of string
| Ldot of t * string
(* | Lapply of t * t *)


let lid str = Lident str


let rec str_of = function
  | Lident s -> s
  | Ldot (lid, s) -> (str_of lid) ^ "." ^ s

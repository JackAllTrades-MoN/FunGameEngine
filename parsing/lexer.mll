{
open Core
open Parser

type error =
  | Illegal_character of char
  | Invalid_literal of string

exception Error of error * Location.t

let error lexbuf e = raise (Error(e, Location.curr lexbuf))
let error_loc loc e = raise (Error(e, loc))

let keyword_table =
  let ls = [
    "context", CONTEXT;
  ] in
  Hashtbl.Poly.of_alist_exn ~size:(List.length ls) ls
}

let newline = ('\013'* '\010')
let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let blank = [' ' '\009' '\012']

let ident = (lowercase | uppercase) identchar*

rule token = parse
  | blank + { token lexbuf }
  | lowercase identchar * as name
      { match Hashtbl.Poly.find keyword_table name with
          None -> LIDENT name
        | Some t -> t }
  | "=" { EQUAL }
  | eof { EOF }
  | (_ as illegal_char)
      { error lexbuf (Illegal_character illegal_char) }
  


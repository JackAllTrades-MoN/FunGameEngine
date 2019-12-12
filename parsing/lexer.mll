{
open Core
open Lexing
open Parser

type error =
  | Illegal_character of char
  | Invalid_literal of string
  | Invalid_directive of string

exception Error of error * Location.t

let error lexbuf e = raise (Error(e, Location.curr lexbuf))
let error_loc loc e = raise (Error(e, loc))

let update_loc lexbuf file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
                 | None -> pos.pos_fname
                 | Some s -> s
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }

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
  | blank + { print_endline "tok: blank"; token lexbuf }
  | newline { print_endline "tok: EOL"; update_loc lexbuf None 1 false 0; EOL }
  | lowercase identchar * as name
      {match Hashtbl.Poly.find keyword_table name with
           None -> print_endline "tok: lident"; LIDENT name
         | Some t -> print_endline "tok: keyword"; t }
  | uppercase identchar * as name { print_endline "tok: uident"; UIDENT name }
  | "#" { HASH }
(*      { print_endline "tok: #";
        let at_beginning_of_line pos = (pos.pos_cnum = pos.pos_bol) in
        if not (at_beginning_of_line lexbuf.lex_start_p)
        then HASH
        else try directive lexbuf with Failure _ -> HASH } *)
  | "=" { print_endline "tok: equal"; EQUAL }
  | ";;" { print_endline "tok:SEMISEMI"; SEMISEMI }
  | eof { EOF }
  | (_ as illegal_char)
      { error lexbuf (Illegal_character illegal_char) }

(*
and directive = parse
  | "quit" { print_endline "tok: quit";
             update_loc lexbuf None (-4) false 0;
             token lexbuf }
  | (identchar * as illegal_dir) { error lexbuf (Invalid_directive illegal_dir) }
*)

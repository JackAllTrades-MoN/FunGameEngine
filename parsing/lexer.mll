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

let string_buffer = Buffer.create 256
let reset_string_buffer () = Buffer.reset string_buffer
let get_stored_string () = Buffer.contents string_buffer

let store_striing s = Buffer.add_string string_buffer s
let store_lexeme lexbuf = store_striing (Lexing.lexeme lexbuf)

let comment_start_loc = ref []

let wrap_comment_lexer comment lexbuf =
  let start_loc = Location.curr lexbuf in
  comment_start_loc := [start_loc];
  reset_string_buffer ();
  let end_loc = comment lexbuf in
  let s = get_stored_string () in
  reset_string_buffer ();
  s, { start_loc with Location.loc_end = end_loc.Location.loc_end }

let keyword_table =
  let ls = [
    "context", CONTEXT;
    "let", LET;
    "rec", REC;
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
  | "." { DOT }
  | "=" { print_endline "tok: equal"; EQUAL }
  | ";;" { print_endline "tok:SEMISEMI"; SEMISEMI }
  | "(*" { let s, loc = wrap_comment_lexer comment lexbuf in COMMENT (s, loc) }
  | eof { EOF }
  | (_ as illegal_char)
      { error lexbuf (Illegal_character illegal_char) }

and comment = parse
    "(*"
      { comment_start_loc := (Location.curr lexbuf) :: !comment_start_loc;
        store_lexeme lexbuf;
        comment lexbuf
      }
  | "*)"
      { match !comment_start_loc with
        | [] -> assert false
        | [_] -> comment_start_loc := []; Location.curr lexbuf
        | _ :: l -> comment_start_loc := l;
                    store_lexeme lexbuf;
                    comment lexbuf
      }
  | _ { store_lexeme lexbuf; comment lexbuf }

(*
and directive = parse
  | "quit" { print_endline "tok: quit";
             update_loc lexbuf None (-4) false 0;
             token lexbuf }
  | (identchar * as illegal_dir) { error lexbuf (Invalid_directive illegal_dir) }
*)

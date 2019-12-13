open Lexing

(*
type _t = 
  { loc_start: position; loc_end: position}

type t = DummyLoc | Loc of _t

let dummy = DummyLoc
*)

type t = { loc_start: position; loc_end: position}

type 'a loc = {
    txt : 'a;
    loc : t;
  }

let in_file name =
  let loc = { dummy_pos with pos_fname = name } in
  { loc_start = loc; loc_end = loc }

let none = in_file "_none_"

let dummy = {loc_start=dummy_pos; loc_end=dummy_pos}

let emb_dummy txt = {txt = txt; loc = dummy}

let curr lexbuf = {
    loc_start = lexbuf.lex_start_p;
    loc_end = lexbuf.lex_curr_p;
  }

let symbol_gloc () = {
    loc_start = Parsing.symbol_start_pos ();
    loc_end = Parsing.symbol_end_pos ();
  }

let mkloc txt loc = { txt; loc }

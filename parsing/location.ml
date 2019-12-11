open Lexing

type _t = 
  { loc_start: position; loc_end: position}

type t = DummyLoc | Loc of _t

type 'a loc = {
    txt : 'a;
    loc : t;
  }

let dummy = DummyLoc

let emb_dummy txt = {txt = txt; loc = dummy}

let curr lexbuf = Loc {
  loc_start = lexbuf.lex_start_p;
  loc_end = lexbuf.lex_curr_p;
}

let symbol_gloc () = Loc {
  loc_start = Parsing.symbol_start_pos ();
  loc_end = Parsing.symbol_end_pos ();
}

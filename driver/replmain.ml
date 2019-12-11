open Core
open ANSITerminal

let print_error msg =
  print_string [red; Bold] "Error: ";
  print_endline (Lazy.force msg)

let repl_start () =
  try
    let lexbuf = Lexing.from_channel In_channel.stdin in
    while true do
      let _result =
        Parser.implementation Lexer.token lexbuf in
      print_endline "parse"
    done
  with Lexer.Error (e, _loc) ->
    print_error (lazy (Printer.str_of_lex_err e));
    exit 0

let () =
  print_endline "\tFunGameEngine REPL version 0.0.0";
  repl_start ()

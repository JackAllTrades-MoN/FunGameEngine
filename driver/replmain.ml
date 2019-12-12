open Core
open ANSITerminal

let print_error msg =
  print_string [red; Bold] "Error: ";
  print_endline (Lazy.force msg)

let print_res msg =
  print_string [blue; Bold] "In OCaml: ";
  print_endline (Lazy.force msg)

let skip_eol f lexbuf =
  match f lexbuf with
  | Parser.EOL -> f lexbuf
  | tok -> tok

let run_derective dir cont =
  if dir.Ast.pdir_name.txt = "quit"
  then cont := false
  else print_error (lazy (Printf.sprintf "Unknown directive %s" dir.Ast.pdir_name.txt))

let repl_start () =
  try
    let lexbuf = Lexing.from_channel In_channel.stdin in
    let cont = ref true in
    while !cont do
      Out_channel.(flush stdout);
      Printf.printf "# "; Out_channel.flush Out_channel.stdout;
      match Parser.toplevel_phrase (skip_eol Lexer.token) lexbuf with
      | Ptop_def structure ->
         print_res (lazy (Toocaml.str_of_structure structure))
      | Ptop_dir dir -> run_derective dir cont
    done
  with Lexer.Error (e, _loc) ->
    print_error (lazy (Printer.str_of_lex_err e));
    exit 0

let () =
  print_endline "\tFunGameEngine REPL version 0.0.0";
  repl_start ()

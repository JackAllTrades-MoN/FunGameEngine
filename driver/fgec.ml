open Core

let rec skip_comment f lexbuf =
  match f lexbuf with
  | Parser.COMMENT _ -> skip_comment f lexbuf
  | tok -> tok

let rec skip_eol f lexbuf =
  match f lexbuf with
  | Parser.EOL -> skip_eol f lexbuf
  | tok -> tok

let main filename () =
  try
    let lexbuf = Lexing.from_channel (In_channel.create filename) in
    let res = Parser.implementation (Lexer.token |> skip_eol |> skip_comment) lexbuf in
    print_endline @@ Toocaml.str_of_structure res
  with Lexer.Error (e, _loc) ->
    print_endline (Printer.str_of_lex_err e);
    exit 0

let readme_msg = "(README)"

let command =
  Command.basic
    ~summary:"Transpile fge files into OCaml programs"
    ~readme:(fun () -> readme_msg)
    Command.Param.(
    map (anon ("filename" %: string)) ~f:main)

let () =
  Command.run ~version:"0.0.0" ~build_info:"EXP" command

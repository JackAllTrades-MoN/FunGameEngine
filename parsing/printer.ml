open Core

let str_of_lex_err = function
  | Lexer.Illegal_character c -> Printf.sprintf "Illegal character (%c)" c
  | Lexer.Invalid_literal s -> Printf.sprintf "Invalid literal (%s)" s

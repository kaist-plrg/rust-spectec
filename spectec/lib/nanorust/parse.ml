open Lexing

let parse (lexbuf : Lexing.lexbuf) = Parser.pgm Lexer.read lexbuf

let parse_file (includes : string list) (filename : string) : Il.Ast.value =
  let program = filename |> open_in |> from_channel in
  ignore includes;
  parse program

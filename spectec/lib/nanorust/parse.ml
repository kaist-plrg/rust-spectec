open Lexing
open Transfer

let parse (lexbuf : Lexing.lexbuf) = Parser.pgm Lexer.read lexbuf

let parse_file (includes : string list) (filename : string) : Il.Ast.value =
  let program = filename |> open_in |> from_channel in
  ignore includes;
  let parse_value = parse program in
  Printf.printf "Parse success \n";
  parse_value |> transfer_pgm

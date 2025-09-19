{
    open Lexing
    open Parser
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
(*let string = [^ '(' ')' '{' '}' '<' '>' '\\' '.' '#' '-' '=' ' ' '\t' '\n' '\t']+*)
let string = ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']*

rule read =
  parse
    | white { read lexbuf }
    | newline { read lexbuf }
    | "." { DOT }
    | ":" { COLON }
    | ":=" { CEQ }
    | "->" { ARROW }
    | "=>" { THICKARROW }
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "{" { LCURLB }
    | "}" { RCURLB }
    | "<" { LANGLE }
    | ">" { RANGLE }
    | "," { COMMA }
    | "=" { EQ }
    | ";" { SEMICOLON }
    | "&" { AMP }
    | "*" { ASTERISK }
    | "//" { skip_line lexbuf }
    | "fn" { FUNC }
    | "let" { LET }
    | "in" { IN }
    | "as" { AS }
    | "where" { WHERE }
    | "struct" { STRUCT }
    | "trait" { TRAIT }
    | "for" { FOR }
    | "Self" { SELF }
    | "∀" { FORALL }
    | "forall" { FORALL }
    | "impl" { IMPL }
    | string { ID (lexeme lexbuf) }
    | eof { EOF }
and skip_line =
  parse
    | newline { new_line lexbuf; read lexbuf }
    | eof { EOF }
    | _ { skip_line lexbuf }
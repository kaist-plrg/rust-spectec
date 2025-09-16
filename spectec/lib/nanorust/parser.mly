(*
  Parser currently writing.
*)
%{
  open Concrete.Ast_utils
%}

%token <string> ID
%token LPAREN "("
%token RPAREN ")"
%token LCURLB "{"
%token RCURLB "}"
%token LANGLE "<"
%token RANGLE ">"
%token ARROW "->"
%token THICKARROW "=>"
%token EQ "="
%token CEQ ":="
%token DOT "."
%token SEMICOLON ";"
%token COLON ":"
%token AMP "&"
%token ASTERISK "*"
%token COMMA ","
%token FUNC "fn"
%token LET "let"
%token IN "in"
%token AS "as"
%token WHERE "where"
%token STRUCT "struct"
%token TRAIT "trait"
%token FOR "for"
%token SELF "Self"
%token FORALL "∀"
%token IMPL "impl"
%token EOF

%nonassoc BELOW_SEMI
%left SEMICOLON

%nonassoc PREC_TERM_END
%left DOT

%start pgm
%type <Il.Ast.value> pgm
%%
identifier:
  | x = ID { [ Term "ID"; Term x; ] #@ "identifier" }

typeIdentifier:
  | x = ID { [ Term "TID"; Term x; ] #@ "typeIdentifier" }

pgm:
  | is = items EOF { is |> wrap_list_v "pgm" }

items: 
  | { [] }
  | i = item is = items { i :: is }

item:
  | f = func { [ NT f ] #@ "item" }
  //| "struct" s = ID tvars = opt_tvars "{" elems = params "}" { Struct (s, tvars, elems) }
  //| "trait" d = ID tvars = opt_tvars "for" "Self"  cons = opt_constraint "{" f = ID ":" scheme = type_scheme "}" { Trait (d, tvars, cons, f, scheme) }
  //| "impl" tvars = opt_tvars d = ID ts = opt_typs "for" t = typ cons = opt_constraint "{" f = func "}" { Impl (tvars, d, ts, t, cons, f) }

func:
  | "fn" f = ID tvars = opt_tvars "(" ps = params ")" "->" t = typ cons = opt_constraint "{" e = term "}"
  { [ Term "FUN"; NT f; Term "{"; NT e; Term "}" ] #@ "func" }

term:
  | "(" ")" { [Term "UNIT"] #@ "term" }
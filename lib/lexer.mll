 (** Lexer **)
{
open Parser
}

rule main = parse
    [' ' '\t']+ { main lexbuf }
  | ['\n'] { EOL }
  | ['a'-'z'] { ID (Lexing.lexeme lexbuf) }
  | "lambda" | '\\' { LAMBDA }
  | "true" { BOOL(true) }
  | "false" { BOOL(false) }
  | "lin" { QUAL(Ast.Lin) }
  | "un" { QUAL(Ast.Un) }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "split" { SPLIT }
  | "in" { IN }
  | "as" { AS }
  | "->" { ARROW }
  | "bool" { TYBOOL }
  | ':' { COLON }
  | '=' { EQ }
  | '<' { LEFT }
  | '>' { RIGHT }
  | '*' { MULTI }
  | '.' { DOT }
  | ',' { COMMA }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | eof { exit 0 }

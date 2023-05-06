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
  | "LIN" { QUAL(Ast.Lin) }
  | "UN" { QUAL(Ast.Un) }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "in" { IN }
  | "split" { SPLIT }
  | "as" { AS }
  | "->" { ALLOW }
  | "bool" { TYBOOL }
  | ':' { COLON }
  | ';' { SEMI }
  | '=' { EQ }
  | '<' { LEFT }
  | '>' { RIGHT }
  | '*' { MULTI }
  | '.' { DOT }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | eof { exit 0 }

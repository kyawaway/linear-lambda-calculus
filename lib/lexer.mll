{
    open Parser;;
}

rule main = parse
            [' ' '\t'] { main lexbuf }
          | [ '\n' ] { EOL }
          | "lambda" | "\\" { LAMBDA }
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
          | "bool" { TYBOOL }
          | "->" { ARROW }
          | '=' { EQ }
          | ';' { SEMI }
          | ',' { COMMA }
          | '<' { LEFT }
          | '>' { RIGHT }
          | '.' { DOT }
          | ':' { COLON }
          | '*' { MULTI }
          | '(' { LPAREN }
          | ')' { RPAREN }
          | ['a'-'z']+ { ID(Lexing.lexeme lexbuf) }

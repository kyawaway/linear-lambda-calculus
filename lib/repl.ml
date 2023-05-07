open Ast
open Parser
open Typecheck

let repl () =
  print_string "\n********** Welcoml to my Interpreter! **********\n";
  let rec loop ctx =
    print_string "> ";
    flush stdout;
    let lexbuf = Lexing.from_channel stdin in
    let name, tm = toplevel Lexer.main lexbuf in
    let ty', ctx' = type_check tm ctx in
    print_context ((name, ty') :: ctx');
    loop ctx'
  in
  loop []

(** Parser **)

%{
    open Ast
%}

%token <string> ID
%token <qualifier> QUAL
%token <bool> BOOL
%token LET EQ LEFT RIGHT SEMI IF THEN ELSE COMMA 
%token LPAREN RPAREN
%token LAMBDA DOT COLON MULTI ARROW IN SPLIT AS TYBOOL
%token EOL

%start parse
%type <Ast.toplevel> parse
%type <Ast.term> term simpleterm
%type <Ast.pretype> qualtype
%%

parse:
      LET ID EQ term SEMI parse { ($2,$4) :: $6 }
    | { [] }
    | EOL { [] }
    ;

term:
      term simpleterm { TmApp ($1,$2) }
    | simpleterm { $1 }
    ;

simpleterm:
      ID { Var $1 }
    | QUAL BOOL { Boolean ($1,$2) }
    | IF t1=term THEN t2=term ELSE t3=term { TmIf (t1,t2,t3) }
    | QUAL LEFT t1=term COMMA t2=term RIGHT { TmPair ($1,t1,t2) }
    | SPLIT t1=term AS v1=ID COMMA v2=ID IN t2=term { TmSplit (t1,v1,v2,t2) }
    | QUAL LAMBDA v=ID COLON q=qualtype DOT t=term { TmAbs ($1,v,q,t) }
    | LPAREN term RPAREN { $2 }
    ;



qualtype:
      QUAL TYBOOL { TyBool $1 }
    | QUAL LPAREN q1=qualtype MULTI q2=qualtype RPAREN { TyPair ($1,q1,q2) }
    | QUAL LPAREN q1=qualtype ARROW q2=qualtype RPAREN { TyFunc ($1,q1,$q2) }
    | LPAREN qualtype RPAREN { $2 }
    ;

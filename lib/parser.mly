%token <string> ID
%token <Ast.qualifier> QUAL
%token <bool> BOOL
%token LET EQ IF THEN ELSE COMMA LEFT RIGHT
%token DOT LAMBDA COLON MULTI ARROW IN SPLIT AS TYBOOL SEMI LPAREN RPAREN
%token EOL

%start toplevel 
%type <Ast.toplevel> toplevel
%type <Ast.term> term term_sub
%type <Ast.pretype> pretype

%%

toplevel:
    LET n=ID EQ tm=term SEMI EOL { (n,tm) }
    | error
    { failwith
        (Printf.sprintf "parse error near characters %d-%d"
           (Parsing.symbol_start ())
           (Parsing.symbol_end ()))
    }
    ;
    
term: 
      term_sub { $1 }
    | tm1=term tm2=term_sub { Ast.TmApp (tm1, tm2) }

term_sub:
      ID { Ast.TmVar $1 }
    | QUAL BOOL { Ast.TmBoolean ($1,$2) }
    | IF tm1=term THEN tm2=term ELSE tm3=term { Ast.TmIf (tm1,tm2,tm3) }
    | q=QUAL LEFT tm1=term COMMA tm2=term RIGHT { Ast.TmPair (q,tm1,tm2) }
    | SPLIT tm1=term AS v1=ID COMMA v2=ID IN tm2=term { Ast.TmSplit (tm1,v1,v2,tm2) }
    | q=QUAL LAMBDA v=ID COLON ty=pretype DOT tm=term { Ast.TmAbs (q,v,ty,tm) }
    | LPAREN term RPAREN { $2 }
    ;

pretype:
      QUAL TYBOOL { Ast.TyBool $1 }
    | q=QUAL LPAREN ty1=pretype MULTI ty2=pretype RPAREN { Ast.TyPair (q,ty1,ty2) }
    | q=QUAL LPAREN ty1=pretype ARROW ty2=pretype RPAREN { Ast.TyFunc (q,ty1,ty2) }
    | LPAREN pretype RPAREN { $2 }
    ;

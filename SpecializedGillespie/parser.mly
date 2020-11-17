/* File parser.mly */
        %token <float> FLOAT
        %token <int> INT
        %token <string> IDENT
        %token LPAREN RPAREN RATES
        %token EOL AND
        %token COMMENTOUT
        %token INITIAL POPULATION PLUS MINUS DIVISION
        %token TIMES UNTIL OBSERVABLES INTERVAL EVERY
        %token SEMICOLON COLON TO STATE REACTIONS COMMA
        %token EOF
        %left AND                                       /* medium precedence */
        %start prog                                     /* the entry point */
        %type <Types.program> prog
        %%  
        prog:  
          reacs init until observables EOF                    { ($1,$2,$3,0,$4,-1.0) }
        | reacs init until observables interval EOF           { ($1,$2,$3,0,$4,$5) }
        | reacs init until  EOF                               { ($1,$2,$3,0,[],-1.0) }
        | reacs init until interval EOF                       { ($1,$2,$3,0,[],$4) }
        | reacs init until every observables EOF              { ($1,$2,$3,$4,$5,-1.0) }
        | reacs init until every observables interval EOF     { ($1,$2,$3,$4,$5,$6) }
        | reacs init until every  EOF                         { ($1,$2,$3,$4,[],-1.0) }
        | reacs init until every interval EOF                 { ($1,$2,$3,$4,[],$5) }
        ;  
          
        reacs:  
         REACTIONS reactions                                   { $2 }
        ;  
        
        init:
         INITIAL STATE  initial_star                           { $3 }
        ;

		    until:  
		     UNTIL FLOAT                                           { $2 }
        ;  
  
        every:  
		     EVERY FLOAT                                           { int_of_float $2 }
        ;

        interval:  
		     INTERVAL FLOAT                                        { $2 }
        ;  
          
         observables:
		     OBSERVABLES observable_star                           { $2 }
		    ;



        reactions:  
          reaction SEMICOLON                                   { [$1] }
        | reaction SEMICOLON reactions                         { $1::$3 }
        ;

        reaction:
          IDENT COLON left_or_right TO left_or_right COMMA exp  { ($1,$3,$5,$7) }
        ;

        left_or_right:
                                                                { [] }
		    | IDENT  LPAREN  FLOAT  RPAREN                          { [($1,int_of_float $3)] }
        | IDENT  LPAREN  FLOAT  RPAREN  PLUS left_or_right      {  ($1,int_of_float $3) :: $6 }
        ;   

        exp:
          FLOAT                                                 { $1 }
        | LPAREN exp TIMES exp RPAREN                           { $2 *. $4 }
        | LPAREN exp PLUS  exp RPAREN                           { $2 +. $4 }
        | LPAREN exp MINUS exp RPAREN                           { $2 -. $4 } 
        ;

        initial_star:
                                                             { [] }
	      | quantity initial_star                              { $1::$2 }
        ;

        quantity:
          FLOAT IDENT LPAREN FLOAT RPAREN                    { (($2,int_of_float $4),int_of_float $1) }
        | IDENT LPAREN FLOAT RPAREN                          { (($1,int_of_float $3),1) }
       ;

        observable_star:
		                                                         { [] }
	     | IDENT observable_star                               { $1::$2 }
       ;
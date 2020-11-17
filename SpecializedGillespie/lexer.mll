(* File lexer.mll *)
{
  exception Illegal_character
  type token = Parser.token
  type position = Lexing.position

  let add_line (lexbuf) =
    let pos:position = Lexing.lexeme_end_p lexbuf in
    let pos:position = {pos with Lexing.pos_lnum = pos.Lexing.pos_lnum+1; Lexing.pos_bol = pos.Lexing.pos_cnum}
    in
       (*IF-OCAML*) lexbuf.Lexing.lex_curr_p <- pos(*ENDIF-OCAML*)

  let remove_ends (s:string) = String.sub s 1 ((String.length s)-2)
  let char_of_string (s:string) = String.get s 0
}
        let float_literal =
  			['0'-'9'] ['0'-'9' '_']*
  			('.' ['0'-'9' '_']* )?
  			(['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?
        let int_literal = ['1'-'9'] ['0'-'9' '_']*
        let letter = ['A'-'Z' '_' 'a'-'z' '~']
		let digit = ['0'-'9']
		let alphanum = digit | letter
		let ident = letter alphanum*

        rule token = parse
          | "//" [^ '\n']*   { token lexbuf }
          | [' ' '\t']       { token lexbuf }     (* skip blanks *)
          | '\n'             { add_line lexbuf; token lexbuf }
          | '\r'             { add_line lexbuf; token lexbuf }
          | "rates"          { Parser.RATES }
          | "until"          { Parser.UNTIL }
          | "->"             { Parser.TO }
          | ":"              { Parser.COLON }
		      | ";"              { Parser.SEMICOLON }
		      | ","              { Parser.COMMA }
          | "reactions"      { Parser.REACTIONS }
          | "and"            { Parser.AND }
          | "initial"        { Parser.INITIAL }
		      | "observables"    { Parser.OBSERVABLES }
          | "interval"       { Parser.INTERVAL }
          | "state"          { Parser.STATE }
          | "interval"       { Parser.INTERVAL }
          | "every"          { Parser.EVERY }
          | "("              { Parser.LPAREN }
          | ")"              { Parser.RPAREN }
          | "*"              { Parser.TIMES }
          | "/"              { Parser.DIVISION }
		      | "+"              { Parser.PLUS }
          | "-"              { Parser.MINUS }
          | float_literal as lxm { Parser.FLOAT(float_of_string lxm) }
          | int_literal as lxm { Parser.INT(int_of_string lxm ) }
          | ident as lxm     { Parser.IDENT(lxm) }
          | eof              { Parser.EOF }
          | _                { raise Illegal_character }

(*****************************************************************************)
{

(*
let run file_name =
  let ic:in_channel = open_in file_name in
  let lexbuf =  Lexing.from_channel ic in
  let this_program = Parser.prog Lexer.token lexbuf in
        main this_program file_name;
        finish file_name
*)

  let format (pos:position) =
    "\nLine " ^ string_of_int pos.Lexing.pos_lnum ^
    " expression starting from the char " ^ string_of_int (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) ^ ". "

  let parse_file (str:string) =
    let ic:in_channel = open_in str in
    let lexbuf = Lexing.from_channel ic in
      try Parser.prog token lexbuf
      with
	  Illegal_character -> failwith (format (Lexing.lexeme_start_p lexbuf) ^ "Illegal character")
	| Parsing.Parse_error -> failwith (format (Lexing.lexeme_start_p lexbuf) ^ "")

  let parse_string (str:string) =
    let lexbuf = Lexing.from_string str in
      try Parser.prog token lexbuf
      with
	  Illegal_character -> failwith (format (Lexing.lexeme_start_p lexbuf) ^ "Illegal character")
	| Parsing.Parse_error -> failwith (format (Lexing.lexeme_start_p lexbuf) ^ "Syntax error")

}
(*****************************************************************************)

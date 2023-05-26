(* File func.mll *)

{

(* 
type token = (* TODO *) 
*) 

type token = ID of string| INT of int|
            VARS| ASGN| SEMI|             
            IF| THEN| ELSE| ENDIF|
            READ| WRITE|
            WHILE| BEGIN| ENDWHILE|
            LSE| LS| EQ| NEQ| 
            LBRA| RBRA|
            METHOD| ENDMETHOD| RETURN| COMMA| 
            EOF

exception LEX of string

}

let int = '-'? ['1'-'9'] ['0'-'9']* | ['0']
let white = [' ' '\t' '\n']+
let id = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*


rule token = parse
    | white          {token lexbuf}
    | ';'            { SEMI }
    | ":="           { ASGN }
    | "vars"         { VARS } 
    | "if"           {IF}
    | "then"         {THEN}
    | "else"         {ELSE}
    | "endif"        {ENDIF}
    | "read"         {READ}
    | "write"        {WRITE}
    | "while"        {WHILE}
    | "begin"        {BEGIN}
    | "endwhile"     {ENDWHILE}
    | "eq"           { EQ }
    | "nEq"          { NEQ }
    | "lessEq"       { LSE }
    | "less"         { LS }
    | '('            { LBRA }
    | ')'            { RBRA }
    | "method"       {METHOD}
    | "endmethod"    {ENDMETHOD}
    | "return"       {RETURN}
    | ','            {COMMA}
    | eof            { EOF }
    | int            { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | id             { ID (Lexing.lexeme lexbuf) }
    | _ { raise (LEX ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
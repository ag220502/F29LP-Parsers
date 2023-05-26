(* File exp.mll *)

{

type token = ID of string | INT of int
           | PLUS  | MINUS | STAR | SLASH 
           | LBRA | RBRA | EOF
exception LEX of string

}

let int = '-'? ['0'-'9'] ['0'-'9']*
let white = [' ' '\t' '\n']+
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
    | white          { token lexbuf }
    | int            { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | id             { ID (Lexing.lexeme lexbuf) }
    | '+'            { PLUS }
    | '-'            { MINUS }
    | '*'            { STAR }
    | '/'            { SLASH }
    | '('            { LBRA }
    | ')'            { RBRA }
    | eof            { EOF }
    | _ { raise (LEX ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
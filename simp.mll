(* File simp.mll - SOLUTION FILE. Copy content into simp.mll *)

{

(* User definitions *)

type token = SEMI | VAR | ASGN | IF | THEN | ELSE
            | WHILE | DO | BEGIN | END | INPUT | PRINT
            | EQ | NEQ | LTE | LT | GTE | GT 
            | ID of string | INT of int
            | PLUS  | MINUS | STAR | SLASH 
            | LBRA | RBRA 
            | EOF
            
exception LexError of string

}

let int = '-'? ['0'-'9'] ['0'-'9']*
let white = [' ' '\t' '\n']+
let id = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*
let linecomment = '#' [^ '\n']*  
let morecomment = "{" [^ '}']*   "}"

rule token = parse
    | linecomment {token lexbuf}
    | morecomment {token lexbuf}
    | white          { token lexbuf }
    | ';'            { SEMI }
    | "VAR"          { VAR } 
    | ":="           { ASGN }
    | "IF"           { IF }
    | "THEN"           { THEN }
    | "ELSE"           { ELSE }
    | "WHILE"           { WHILE }
    | "DO"           { DO }
    | "BEGIN"           { BEGIN }
    | "END"           { END }
    | "INPUT"           { INPUT }
    | "PRINT"           { PRINT }
    | '='           { EQ }
    | "!="           { NEQ }
    | "<="           { LTE }
    | '<'           { LT }
    | ">="           { GTE }
    | '>'           { GT }
    | '+'            { PLUS }
    | '-'            { MINUS }
    | '*'            { STAR }
    | '/'            { SLASH }
    | '('            { LBRA }
    | ')'            { RBRA }
    | eof            { EOF }
    | int            { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | id             { ID (Lexing.lexeme lexbuf) }
    | _ { raise (LexError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
#load "simp.cmo";;

open Simp;;

let rec stream_to_list buffer = 
    match Simp.token buffer with 
    | EOF -> []
    | x -> x :: stream_to_list buffer
    
    
exception SyntaxError of string

type op = Plus | Minus | Mult | Div 

type exp = Id of string | Numb of int | Op of exp * op * exp | Neg of exp

type cond = Eq | Neq | Lte | Lt | Gte | Gt 
type condexp = Cop of exp * cond * exp
                                          
type cmd = Asgn of string * exp 
         | Ite of condexp * cmd * cmd | If of condexp * cmd 
         | While of condexp * cmd
         | Begin of program 
         | Input of string
         | Print of exp 
         
and program = Program of string list * cmd list


let parse_token (x : token) (xs : token list) = match xs with 
| y :: ys -> if (x == y) then ys else raise (SyntaxError "Token expected.")
| _ -> raise (SyntaxError "Token expected.") 

let rec parse_exp (xs : token list) : exp * token list = let 
  (e1, xs') = parse_term xs in 
  match xs' with 
  | PLUS :: xs'' -> let 
      (e2, xs''') = parse_exp xs'' 
      in (Op (e1, Plus, e2), xs''')
  | MINUS :: xs'' -> let 
      (e2, xs''') = parse_exp xs'' 
      in (Op (e1, Minus, e2), xs''')
  | _ -> (e1, xs') 
           
and parse_term (xs : token list) : exp * token list = let 
  (e1, xs') = parse_factor xs in 
  match xs' with 
  | STAR :: xs'' -> let 
    (e2, xs''') = parse_term xs''
      in (Op (e1, Mult, e2), xs''') 
  | SLASH :: xs'' -> let 
    (e2, xs''') = parse_term xs''
      in (Op (e1, Div, e2), xs''')    
  | _ -> (e1, xs')
  
and parse_factor (xs : token list) : exp * token list = match xs with 
  | MINUS :: xs' -> let (e, xs'') = parse_base xs' in 
                    (Neg e, xs'')
  | _ -> parse_base xs
  
and parse_base (xs : token list) : exp * token list = match xs with 
  | ID x :: xs' -> (Id x, xs')
  | INT x :: xs' -> (Numb x , xs')
  | LBRA :: xs' -> (let 
        (e, xs'') = parse_exp xs' in let
         xs''' = parse_token RBRA xs''
      in (e, xs'''))
  | _ -> raise (SyntaxError "Expected ID, INT or LBRA.") 
  
let parse_comp (ts : token list) : cond * token list = match ts with 
    | LT :: ts' -> (Lt, ts')
    | LTE :: ts' -> (Lte, ts')
    | EQ :: ts' -> (Eq, ts')
    | NEQ :: ts' ->  (Neq, ts')
    | GT :: ts' -> (Gt, ts')
    | GTE :: ts' -> (Gte, ts')
    | _ -> raise (SyntaxError "Comparison token expected.")
    
let parse_cond (ts : token list) : condexp * token list = let 
        (e1, ts') = parse_exp ts in let
        (c, ts'') = parse_comp ts' in let 
        (e2, ts''') = parse_exp ts'' in 
        (Cop (e1, c, e2), ts''')  

let parse_declaration (ts : token list) : string * token list = match ts with 
  | VAR :: ID x :: ts' -> (x, ts')
  | _ ->  raise (SyntaxError "Declaration expected.")

let rec parse_declarations (ts : token list) : string list * token list = let 
  (dcl, ts') = parse_declaration ts in let 
  ts'' = parse_token SEMI ts' in 
  match ts'' with 
  | VAR :: _ -> (let (dcls, ts''') = parse_declarations ts'' in 
                  (dcl :: dcls, ts''')
                  )
  | _ -> ([dcl], ts'')


let parse_assign x ts = let 
    ts' = parse_token ASGN ts in let 
    (e, ts'') = parse_exp ts' in 
    ((Asgn (x, e)), ts'')

let parse_input ts = match ts with 
    | ID x :: ts' -> (Input x, ts')
    | _ -> raise (SyntaxError "Identifier expected.")

let parse_print ts = let 
    (e, ts') = parse_exp ts in 
    (Print e, ts')


let rec parse_program (ts : token list) : program * token list = match ts with 
    | VAR :: _ -> let 
                  (dcls, ts') = parse_declarations ts in let 
                  (cs, ts'') = parse_commands ts' in 
                  (Program (dcls, cs), ts'')
    | _ -> let 
          (cs, ts'') = parse_commands ts in 
          (Program ([], cs), ts'')

and parse_commands (ts : token list) : cmd list * token list = let 
    (c, ts') = parse_command ts in 
    match ts' with 
    | SEMI :: ts'' -> let 
                      (cs, ts''') = parse_commands ts''
                      in (c :: cs, ts''')
    | _ -> ([c], ts')

and parse_command (ts : token list) : cmd * token list = match ts with 
  | ID x :: ts' -> parse_assign x ts'
  | INPUT :: ts' -> parse_input ts'
  | PRINT :: ts' -> parse_print ts'
  | IF :: ts' -> parse_if ts'
  | WHILE :: ts' -> parse_while ts'
  | BEGIN :: ts' -> parse_block ts'
  | _ -> raise (SyntaxError "Command expected.")
    
and parse_if (ts : token list) : cmd * token list = let 
    (b, ts') = parse_cond ts in let 
    ts'' = parse_token THEN ts' in let
    (c1, ts''') = parse_command ts'' in 
        match ts''' with 
        | ELSE :: ts'''' -> let (c2, ts''''') = parse_command ts'''' in 
                           (Ite (b, c1, c2), ts''''')
        | _ -> (If (b, c1), ts''')

and parse_while (ts : token list) : cmd * token list = let 
    (b, ts') = parse_cond ts in let 
    ts'' = parse_token DO ts' in let 
    (c, ts''') = parse_command ts'' in 
    (While (b, c), ts''')

and parse_block (ts : token list) : cmd * token list = let 
    (p, ts') = parse_program ts in let 
    ts'' = parse_token END ts' in 
    (Begin p, ts'')        
    
    
(* This will define maps with strings as key *)
module Env = Map.Make(String)

(* Env.empty denotes the empty environment. 
We can add elements to an environment via Env.add.
This is the environment which only binds “a” to 3. *)
let example_env = Env.add "a" 3 Env.empty;;

(* We can look up elements in an environment via Env.find.
Env.find throws an exception if the key does not exist.*)
Env.find "a" example_env;;


let parse_string s = let 
        lexed = stream_to_list (Lexing.from_string s) in let 
        parsed = parse_program lexed in
        match parsed with 
        | (parsed', []) -> parsed'
        | _ -> raise (SyntaxError "Not empty token list.") 

exception RuntimeError of string

let rec eval (e : exp) env : int = match e with 
    | Id x -> Env.find x env 
    | Numb n -> n 
    | Neg e -> - (eval e env)
    | Op (e1, Plus, e2) -> eval e1 env + eval e2 env
    | Op (e1, Minus, e2) -> eval e1 env - eval e2 env
    | Op (e1, Mult, e2) -> eval e1 env * eval e2 env
    | Op (e1, Div, e2) ->  (match (eval e2 env) with 
                          | 0 -> raise (RuntimeError "Divison by 0")
                          | _ -> eval e1 env / eval e2 env)
                          
let eval_condexp (e : condexp) env : int = match e with                          
    | Cop (e1, Eq, e2) -> if (eval e1 env = eval e2 env) then 1 else 0
    | Cop (e1, Neq, e2) -> if (eval e1 env = eval e2 env) then 0 else 1
    | Cop (e1, Lte, e2) -> if (eval e1 env <= eval e2 env) then 1 else 0
    | Cop (e1, Lt, e2) -> if (eval e1 env < eval e2 env) then 1 else 0
    | Cop (e1, Gte, e2) -> if (eval e1 env >= eval e2 env) then 1 else 0
    | Cop (e1, Gt, e2) -> if (eval e1 env > eval e2 env) then 1 else 0

let rec eval_cmd (c : cmd) env  = match c with 
    | Asgn (x, e) -> Env.add x (eval e env) env
    | If (e, c) -> (match (eval_condexp e env) with 
                        | 1 -> eval_cmd c env 
                        | 0 -> env
                        | _ -> raise (RuntimeError "Error in condition of If")
                        )
    | Ite (e, c1, c2) -> (match (eval_condexp e env) with 
                        | 1 -> eval_cmd c1 env 
                        | 0 -> eval_cmd c2 env
                        | _ -> raise (RuntimeError "Error in condition of If"))
    | While (e, c) -> (match (eval_condexp e env) with 
                        | 1 -> eval_cmd (While (e, c)) (eval_cmd c env)
                        | _ -> env)
    | Begin p -> let _ = eval_program p env in env
    | Print e -> let _ = print_endline ("OUTPUT:" ^ string_of_int (eval e env)) in env
    | Input x -> let _ = print_endline ("Input is not supported by Jupyter Notebooks, assuming input 0.") in 
                         Env.add x 0 env
    
and eval_program p env = match p with 
    | Program (xs, cmds) -> eval_commands cmds env

and eval_commands (cs : cmd list) env = match cs with 
    | [] -> env
    | c :: cs -> eval_commands cs (eval_cmd c env)
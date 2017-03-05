(*  
	CMSC330 Fall 2016
	This ocaml code reads a C code and properly indents it
	
	compile for debug:
		ocamlc -g Str.cma smallc.ml 
	
	@author: Anwar Mamat
	@date: 10/15/2016
*)

#load "str.cma"

type data_type =
	|Type_Int
;;

(* Use this as your abstract syntax tree *)

type ast =
  | Id of string
  | Num of int
  | Define of data_type * ast
  | Assign of ast * ast
  | List of ast list
  | Fun of data_type * string * ast * ast   (* return type * function name * argument list * statement list *)
  | Sum of ast * ast
  | Greater of ast * ast
  | Equal of ast * ast
  | Less of ast * ast
  | Mult of ast * ast
  | Pow of  ast * ast
  | Print of ast
  | If of ast * ast * ast	(* cond * if brach * else branch *)
  | While of ast * ast
  | Paren of ast
  
;;

type token =
 | Tok_Id of string
 | Tok_Num of int
 | Tok_String of string
 | Tok_Assign
 | Tok_Greater
 | Tok_Less
 | Tok_Equal
 | Tok_LParen
 | Tok_RParen
 | Tok_Semi
 | Tok_Main
 | Tok_LBrace
 | Tok_RBrace
 | Tok_Int 
 | Tok_Float
 | Tok_Sum
 | Tok_Mult
 | Tok_Pow
 | Tok_Print
 | Tok_If
 | Tok_Else
 | Tok_While
 | Tok_END
 
(* tokens *)
let re_lparen = Str.regexp "("
let re_rparen = Str.regexp ")"
let re_lbrace = Str.regexp "{"
let re_rbrace = Str.regexp "}"
let re_assign = Str.regexp "="
let re_greater = Str.regexp ">"
let re_less = Str.regexp "<"
let re_equal = Str.regexp "=="
let re_semi = Str.regexp ";"
let re_int = Str.regexp "int"
let re_float = Str.regexp "float"
let re_printf = Str.regexp "printf"
let re_main = Str.regexp "main"
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let re_num = Str.regexp "[-]?[0-9]+"
let re_string = Str.regexp "\"[^\"]*\""
let re_whitespace = Str.regexp "[ \t\n]"
let re_add = Str.regexp "+"
let re_mult = Str.regexp "*"
let re_pow = Str.regexp "\\^"
let re_if = Str.regexp "if"
let re_else = Str.regexp "else"
let re_while = Str.regexp "while"


exception Lex_error of int
exception Parse_error of int ;;
exception IllegalExpression of string

let tokenize s =
 let rec tokenize' pos s =
   if pos >= String.length s then
     [Tok_END]
   else begin
     if (Str.string_match re_lparen s pos) then
       Tok_LParen::(tokenize' (pos+1) s)
     else if (Str.string_match re_rparen s pos) then
       Tok_RParen::(tokenize' (pos+1) s)
     else if (Str.string_match re_add s pos) then
       Tok_Sum::(tokenize' (pos+1) s)
     else if (Str.string_match re_mult s pos) then
       Tok_Mult::(tokenize' (pos+1) s)
     else if (Str.string_match re_equal s pos) then
       Tok_Equal::(tokenize' (pos+2) s)
     else if (Str.string_match re_if s pos) then
       Tok_If::(tokenize' (pos+2) s)
     else if (Str.string_match re_else s pos) then
       Tok_Else::(tokenize' (pos+4) s)    
     else if (Str.string_match re_while s pos) then
       Tok_While::(tokenize' (pos+5) s)       
	else if (Str.string_match re_pow s pos) then
       Tok_Pow::(tokenize' (pos+1) s)
    else if (Str.string_match re_printf s pos) then
       Tok_Print::tokenize' (pos+6) s
    else if (Str.string_match re_lbrace s pos) then
       Tok_LBrace::(tokenize' (pos+1) s)
    else if (Str.string_match re_rbrace s pos) then
       Tok_RBrace::(tokenize' (pos+1) s)
    else if (Str.string_match re_assign s pos) then
       Tok_Assign::(tokenize' (pos+1) s)
    else if (Str.string_match re_greater s pos) then
       Tok_Greater::(tokenize' (pos+1) s)
    else if (Str.string_match re_less s pos) then
       Tok_Less::(tokenize' (pos+1) s)
    else if (Str.string_match re_semi s pos) then
       Tok_Semi::(tokenize' (pos+1) s)
    else if (Str.string_match re_int s pos) then
       Tok_Int::(tokenize' (pos+3) s)
    else if (Str.string_match re_float s pos) then
       Tok_Float::(tokenize' (pos+5) s)
    else if (Str.string_match re_main s pos) then
       Tok_Main::(tokenize' (pos+4) s)
     else if (Str.string_match re_id s pos) then
       let token = Str.matched_string s in
       let new_pos = Str.match_end () in
       (Tok_Id token)::(tokenize' new_pos s)
     else if (Str.string_match re_string s pos) then
       let token = Str.matched_string s in
       let new_pos = Str.match_end () in
       let tok = Tok_String (String.sub token 1 ((String.length token)-2)) in
       tok::(tokenize' new_pos s)
     else if (Str.string_match re_num s pos) then
       let token = Str.matched_string s in
       let new_pos = Str.match_end () in
       (Tok_Num (int_of_string token))::(tokenize' new_pos s)
     else if (Str.string_match re_whitespace s pos) then
       tokenize' (Str.match_end ()) s
     else
       raise (Lex_error pos)
   end
 in
 tokenize' 0 s
 
 
 (* C Grammar *)
 (* 
 
 basicType-> 'int'
  mainMethod-> basicType 'main' '(' ')' '{' methodBody '}'
  methodBody->(localDeclaration | statement)*
  localDeclaration->basicType ID ';'
  statement->
    whileStatement
    |ifStatement
    |assignStatement
    |printStatement
  
  assignStatement->ID '=' exp ';'
  ifStatement -> 'if' '(' exp ')'  '{' ( statement)* '}'  ( 'else' '{'( statement)* '}')?
  whileStatement -> 'while''(' exp ')' '{'(statement )*'}'
  printStatement->'printf' '(' exp ')' ';'
  exp -> additiveExp (('>'  | '<'  | '==' ) additiveExp )*
  additiveExp -> multiplicativeExp ('+' multiplicativeExp)*
  multiplicativeExp-> powerExp ( '*' powerExp  )*
  powerExp->primaryExp ( '^' primaryExp) *
  primaryExp->'(' exp ')' | ID 
  ID->( 'a'..'z' | 'A'..'Z') ( 'a'..'z' | 'A'..'Z' | '0'..'9')*
  WS-> (' '|'\r'|'\t'|'\n') 



*)

(*----------------------------------------------------------
  function lookahead : token list -> (token * token list)
	Returns tuple of head of token list & tail of token list
*)

let lookahead tok_list = match tok_list with
        [] -> raise (IllegalExpression "lookahead")
        | (h::t) -> (h,t)
;;        



(* -------------- Helper Functions ----------------------- *)

let get_Id (tok,lst) = match tok with
  |Tok_Id id -> id
  | _ -> raise (IllegalExpression "get_Id")


let match_tok tok tok_list =
  match tok_list with
    | (h::t) when tok = h -> t
    | _ -> raise (IllegalExpression "match_tok")

let match_tok1 tok tok_list =
  match tok_list with
    | (h::t) when tok = h -> t
    | _ -> raise (IllegalExpression "FOUND IT")

let rec unwrap lst = 
  let rec aux lst1 = 
    match lst1 with
      |h::t -> if (match h with |List(h1) -> true |_-> false) 
      then (let h1=(match h with |List(h1) -> h1) in (aux h1)@(aux t) ) 
      else [h]@(aux t)
      |[] -> []
  in
  match lst with
    |List(l) -> let x = aux l in List(x)

(* ---------------------Parse Functions--------------------*)

let rec parse_Function lst = 
  let f = lookahead lst in
  match f with
    |(Tok_Int,_) ->  (
    let n = match_tok Tok_Int lst in
    let n1 = match_tok Tok_Main n in
    let n2 = match_tok Tok_LParen n1 in
    let n3 = match_tok Tok_RParen n2 in
    let n4 = match_tok Tok_LBrace n3 in
    let (ast1,lst2) = (parse_methodBody n4) in 
    let n5 = match_tok Tok_RBrace lst2 in
    let n6 = match_tok Tok_END n5 in
    (Fun(Type_Int,"main",List[],ast1), n6)
    )
    |_ -> raise (IllegalExpression "parse_Function")

and parse_methodBody lst = 
  let f = lookahead lst in
  match f with
    |(Tok_Int,_) -> (  (* localDeclaration  *)
      let (ast1,lst1) = (parse_localDecl lst) in  
      let (ast2,lst2) = parse_methodBody lst1 in 
        if ast2 = List[] 
        then (List[ast1],lst2)
        else (unwrap(List[ast1;ast2]),lst2)
    )
    
    |(Tok_While,_) ->  ( (* whileStatement *)
      let (ast1,lst1) = (parse_statement lst) in
      let (ast2, lst2) = (parse_methodBody lst1) in 
        if ast2 = List[] 
        then (List[ast1],lst2)
      else (unwrap(List[ast1;ast2]),lst2)
      )
    
    |(Tok_If,_) -> ((* ifStatement *)
      let (ast1,lst1) = parse_statement lst in
      let (ast2,lst2) = parse_methodBody lst1 in
      if ast2 = List[] 
        then (List[ast1],lst2)
      else (unwrap(List[ast1;ast2]),lst2)

    )
    
    |(Tok_Print,_) -> ((* printStatement *)
      let (ast1,lst1) = parse_statement lst in
      let (ast2,lst2) = parse_methodBody lst1 in
      if ast2 = List[] 
        then (List[ast1],lst2)
      else (unwrap(List[ast1;ast2]),lst2)
    )
    
    |(Tok_Id id, _) -> ( (* assignStatement *)
      let (ast1,lst1) = parse_statement lst in  
      let (ast2,lst2) = parse_methodBody lst1 in 
        if ast2 = List[]
        then (List[ast1],lst2)
      else (unwrap(List[ast1;ast2]),lst2)
    )

    |(Tok_RBrace,_) -> (
      (List[],lst)
    )
    |_ -> raise (IllegalExpression "parse_methodBody")
      
and parse_localDecl lst = 
  let f = lookahead lst in
  match f with
  |(Tok_Int,_) -> (
      let n = match_tok Tok_Int lst in 
        let id = get_Id (lookahead n) in 
      let n1 = match_tok (Tok_Id id) n in
      let n2 = match_tok Tok_Semi n1 in
      (Define(Type_Int,Id id), n2)
    )
  | _ -> raise (IllegalExpression "parse_localDecl")

and parse_statement lst = 
  let f = lookahead lst in
  match f with
    |(Tok_While,_) ->  ( (* whileStatement *)
      let (ast1,lst1) = (parse_whileStatement lst) in
      (ast1,lst1)
      )
    
    |(Tok_If,_) -> ((* ifStatement *)
      let (ast1,lst1) = parse_ifStatement lst in
      (ast1,lst1)
    )
    
    |(Tok_Print, lst1) -> ((* printStatement *)
      let (ast1,lst1) = parse_printStatement lst in
      (ast1,lst1)
    )
    |(Tok_Id id, _) -> ( (* assignStatement *)
      let (ast1,lst1) = parse_assignStatement lst in  
      (ast1,lst1)
    )

and parse_whileStatement lst = 
  let f = lookahead lst in 
  match f with
    |(Tok_While,_) -> (
      let n = match_tok Tok_While lst in
      let n1 = match_tok Tok_LParen n in
      let (ast1,lst1) = (parse_exp n1) in
      let n2 = match_tok Tok_RParen lst1 in
      let n3 = match_tok Tok_LBrace n2 in
      let (ast2,lst2) = parse_methodBody n3 in   (* could cause problems if we
                                                        declare after this *)     
      let n4 = match_tok Tok_RBrace lst2 in
      (While(ast1,ast2),n4)
    )
   | _ -> raise (IllegalExpression "parse_whileStatement") 

and parse_assignStatement lst =
  let f = lookahead lst in
  match f with 
    |(Tok_Id id,_) -> (
      let n = match_tok (Tok_Id id) lst in
      let n1 = match_tok Tok_Assign n in
      let (ast1,lst1) = parse_exp n1 in
      let n2 = match_tok Tok_Semi lst1 in
      (Assign(Id id, ast1),n2)
    )
    |_-> raise (IllegalExpression "parse_assignStatement")

and parse_ifStatement lst = 
  let f = lookahead lst in
  match f with
    |(Tok_If,_) -> (
      let n = match_tok Tok_If lst in
      let n1 = match_tok Tok_LParen n in
      let (ast1,lst1) = parse_exp n1 in
      let n2 = match_tok Tok_RParen lst1 in
      let n3 = match_tok Tok_LBrace n2 in
      let (ast2,lst2) = parse_ifBody n3 in 
      (*let n4 = match_tok Tok_RBrace lst2 in *)
      if (match (lookahead lst2) with |(Tok_Else,_)->true |_->false  ) then
        let (ast3,lst3) = parse_else lst2 in
        (If(ast1,ast2,ast3),lst3)
      else (If(ast1,ast2,List[]),lst2)
    )
    |_ -> raise (IllegalExpression "parse_ifStatement")

and parse_ifBody lst = 
  let f = lookahead lst in
  match f with
    |(Tok_RBrace,_) -> (
      let n = match_tok Tok_RBrace lst in
      (List[],n)
    )
    |_-> (
      let (ast1,lst1) = parse_statement lst in 
      let (ast2,lst2) = parse_ifBody lst1 in
      if ast2 = List[]
        then (List[ast1],lst2)
      else (unwrap(List[ast1;ast2]),lst2)
    )

and parse_else lst = 
  let f = lookahead lst in
  match f with
    |(Tok_Else,_) -> (
      let n = match_tok Tok_Else lst in
      let n1 = match_tok Tok_LBrace n in
      let (ast1,lst1) = parse_ifBody n1 in
      (ast1,lst1)
    )
    |_-> raise (IllegalExpression "parse_else")

and parse_printStatement lst = 
  let f = lookahead lst in
  match f with
    |(Tok_Print,_) -> (
      let n = match_tok Tok_Print lst in
      let n1 = match_tok Tok_LParen n in
      let (ast1,lst1) = parse_exp n1 in
      let n2 = match_tok Tok_RParen lst1 in
      let n3 = match_tok Tok_Semi n2 in
      (Print(ast1),n3)
    ) 

and parse_exp lst =
  let (ast1,lst1) = parse_additiveExp lst in 
  let f = lookahead lst1 in
  match f with
    |(Tok_Greater,_) -> (
      let n = match_tok Tok_Greater lst1 in
      let (ast2,lst2) = parse_exp n in
      (Greater(ast1,ast2),lst2)      
    ) 
    |(Tok_Less,_) -> (
      let n = match_tok Tok_Less lst1 in
      let (ast2,lst2) = parse_exp n in
      (Less(ast1,ast2),lst2) 
    )
    |(Tok_Equal,_) -> (
      let n = match_tok Tok_Equal lst1 in
      let (ast2,lst2) = parse_exp n in
      (Equal(ast1,ast2),lst2) 
    )
    |_ -> (ast1,lst1) (*raise (IllegalExpression "parse_exp")
      *)
and parse_additiveExp lst =
  let (ast1,lst1) = parse_multiplicativeExp lst in 
  let f = lookahead lst1 in
  match f with
    |(Tok_Sum,_) -> (
      let n = match_tok Tok_Sum lst1 in
      let (ast2,lst2) = parse_additiveExp n in 
      (Sum(ast1,ast2),lst2)
    )
    |_-> (ast1,lst1) (*raise (IllegalExpression "parse_additiveExp")
*)
and parse_multiplicativeExp lst =
  let (ast1,lst1) = parse_powerExp lst in 
  let f = lookahead lst1 in
  match f with
    |(Tok_Mult,_) -> (
      let n = match_tok Tok_Mult lst1 in
      let (ast2,lst2) = parse_multiplicativeExp n in
      (Mult(ast1,ast2),lst2)
    )
    |_-> (ast1,lst1) (*raise (IllegalExpression "parse_multiplicativeExp")
    *)
and parse_powerExp lst = 
  let (ast1,lst1) = parse_primaryExp lst in 
  let f = lookahead lst1 in
  match f with
    |(Tok_Pow,_) -> (
      let n = match_tok Tok_Pow lst1 in
      let (ast2,lst2) = parse_powerExp n in
      (Pow(ast1,ast2),lst2)
    )
    |_-> (ast1,lst1) (*raise (IllegalExpression "parse_powerExp")
*)
and parse_primaryExp lst =
  let f = lookahead lst in  
  match f with
  |(Tok_LParen,_) -> (
    let n = match_tok Tok_LParen lst in
    let (ast1,lst1) = parse_exp n in
    let n1 = match_tok Tok_RParen lst1 in
    (Paren(ast1),n1)
  )
  |(Tok_Id id,_) -> (
    let n = match_tok (Tok_Id id) lst in
    (Id id,n)
  )
  |(Tok_Num num,_) -> (
    let n = match_tok (Tok_Num num) lst in
    (Num num,n)
  )
  |_-> raise (IllegalExpression "parse_primaryExp")


(* --------------------End of my code----------------------*)

exception Error of int ;;

let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let tok_to_str t = ( match t with
          Tok_Num v -> string_of_int v
        | Tok_Sum -> "+"
        | Tok_Mult ->  "*"
        | Tok_LParen -> "("
        | Tok_RParen -> ")"
		| Tok_Pow->"^"
        | Tok_END -> "END"
        | Tok_Id id->id
		| Tok_String s->s
		| Tok_Assign->"="
		 | Tok_Greater->">"
		 | Tok_Less->"<"
		 | Tok_Equal->"=="
		 | Tok_Semi->";"
		 | Tok_Main->"main"
		 | Tok_LBrace->"{"
		 | Tok_RBrace->"}"
		 | Tok_Int->"int" 
		 | Tok_Float->"float"
		 | Tok_Print->"printf"
		 | Tok_If->"if"
		 | Tok_Else->"else"
		 | Tok_While-> "while"
    )

let print_token_list tokens =
	print_string "Input token list = " ;
	List.iter (fun x -> print_string (" " ^ (tok_to_str x))) tokens;
	print_endline ""
;;
	
(* -------------- Your Code Here ----------------------- *)

let rec print_underscore pos = 
  match pos with
  |0 -> print_string ""
  |_-> print_string "_"; print_underscore (pos-1)

let rec pretty_print pos x =
	 match x with
   |Fun(Type_Int,main,x1,x2) -> print_string "int main(){\n"; pretty_print 4 x2; print_string "}"
   |List(lst) -> aux_print_list pos lst
   |Id(id) -> print_string id
   |Num(num) -> print_int num
   |Define(data_type,x1) -> print_underscore pos; 
      print_string "int "; pretty_print pos x1; print_string ";\n"
   |Assign(x1,x2) -> print_underscore pos; pretty_print pos x1;
    print_string " = "; pretty_print pos x2; print_string ";\n"
   |Sum(x1,x2) -> pretty_print pos x1; print_string " + "; 
      pretty_print pos x2
   
   |Greater(x1,x2) -> pretty_print pos x1; print_string " > "; 
      pretty_print pos x2
   |Equal(x1,x2) -> pretty_print pos x1; print_string " == "; 
      pretty_print pos x2
   |Less(x1,x2) -> pretty_print pos x1; print_string " < "; 
      pretty_print pos x2
   |Mult(x1,x2) -> pretty_print pos x1; print_string " * "; 
      pretty_print pos x2
   |Pow(x1,x2) -> pretty_print pos x1; print_string " ^ "; 
      pretty_print pos x2
   |Print(x1) -> print_underscore pos; print_string "printf(";
      pretty_print pos x1; print_string ");\n"
   |If(x1,x2,x3) -> print_underscore pos; print_string "if(";
    pretty_print pos x1; print_string "){\n"; 
    pretty_print (pos+4) x2; print_underscore pos; 
    print_string "}"; 
    if (match x3 with |List[]->false |List(lst)->true |_->false) 
    then (print_string "else{\n"; pretty_print (pos+4) x3; 
    print_underscore (pos); print_string "}\n")
    else print_string "\n"
   |While(x1,x2) -> print_underscore pos; print_string "while(";
     pretty_print pos x1; print_string "){\n";
     pretty_print (pos+4) x2; print_underscore pos;
     print_string "}\n" 
   |Paren(x1) -> print_string "("; pretty_print pos x1; 
      print_string ")"
   |_-> print_string ""

and aux_print_list pos x = 
    match x with
    |h::t -> pretty_print pos h; aux_print_list pos t
    |[]-> ()

(* ----------------------------------------------------- *)


(*
you can test your parser and pretty_print with following code 
*)

(*

let prg1 = read_lines "main.c";;
let code = List.fold_left (fun x y->x^y) "" prg1;;	
let t = tokenize code;;
let (a,b)=parse_Function t;;

*)
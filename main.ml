open Unif
open Genlex

let rec i2n n = if n=0 then O else S (i2n (n-1))

let rec n2i = function 
    O -> 0 
  | S n -> succ (n2i n)

let nb_fun = ref 0
let h_fun = Hashtbl.create 103
let h_fun_inv = Hashtbl.create 103


let nb_var = ref 0
let h_var = Hashtbl.create 103
let h_var_inv = Hashtbl.create 103

let lexer = make_lexer ["(" ; "," ; ")"]

let get_nf f = 
  try 
    Hashtbl.find h_fun f 
  with Not_found -> 
    let p = !nb_fun in 
    nb_fun:=p+1;
    Hashtbl.add h_fun f p; 
    Hashtbl.add h_fun_inv p f;
    p

let get_nv v = 
  try 
    Hashtbl.find h_var v 
  with Not_found -> 
    let p = !nb_var in 
    nb_var:=p+1;
    Hashtbl.add h_var v p; 
    Hashtbl.add h_var_inv p v;
    p

let rec parse_expr = parser
    [< 'Ident n ; p = parse_rest >] -> 
      (match p with 
	   Some a -> 
	     (match a with 
		  None -> C (i2n (get_nf n)) 
		| Some e  -> Root (i2n (get_nf n), e))
	 |  None -> V (i2n (get_nv n)))

and parse_rest = parser 
  | [< 'Kwd "("; a = parse_args ; 'Kwd ")" >] -> Some a
  | [<>] -> None

and parse_args = parser
  | [< n = parse_many_args >] -> Some n 
  | [< >] -> None 
	 

and parse_many_args = parser
  | [< n = parse_expr ; p = parse_args_rest >] -> 
      (match p with 
	| Some e -> ConsArg (n,e)
	| None -> n)

and parse_args_rest = parser 
  | [< 'Kwd "," ; p = parse_many_args >] -> Some p
  | [< >] -> None

let parse_expression = parser [< e = parse_expr; _ = Stream.empty >] -> e

let expr_of_string s = 
  parse_expression (lexer (Stream.of_string s))  

let rec string_of_expr = function 
    V n -> 
      Hashtbl.find h_var_inv (n2i n)
  | C n -> (Hashtbl.find h_fun_inv (n2i n))^"()"
  | Root(n,e)-> 
      (Hashtbl.find h_fun_inv (n2i n))^"("^
      (string_of_expr e)^")"
  | ConsArg (e,e')->
      (string_of_expr e)^","^(string_of_expr e')
      

let _ = 
   let e1 = expr_of_string (Sys.argv.(1))
   and e2 = expr_of_string (Sys.argv.(2)) 
   in 
   match (unif_proof e1 e2) with 
     | Unif_fail -> 
	 print_string "Unification failed !\n";exit 1
     | Unif_succeed f -> 
	 print_string "Unification gives : "; 
	 print_string (string_of_expr (subst f e1)); 
	 print_newline();
	 exit 0

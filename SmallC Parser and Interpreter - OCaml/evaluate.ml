let rec pow num exp = 
	match exp with
		|0-> 1
		|_-> num*(pow num (exp-1))
;;

let rec eval env x =  
	let rec aux ast = 
		match ast with
		|Fun(data_type,main,x1,x2) -> aux x2
	   	|List(lst) -> (match lst with 
			|[]-> 0
	   		|h::t-> aux h; aux (List(t)) 
	   		)
	   	|Id(id) -> if Hashtbl.mem env id && not((Hashtbl.find env id)=max_int) then (Hashtbl.find env id) 
	   		else raise (IllegalExpression "Illegal access exception") 
	   	|Num(num) -> num
	   	|Define(data_type,id) -> let id2 = (match id with |Id(id1)->id1) in
	   		if not(Hashtbl.mem env id2) 
	   		then (Hashtbl.add env id2 max_int; 1) (*max_int to signify uninitialized variable*)
	   		else raise (IllegalExpression "Illegal Define Exception")
	   	|Assign(id,x2) -> let id2 = (match id with |Id(id1)->id1) in 
	   		let exp = aux x2 in Hashtbl.add env id2 exp; 1
	   	|Sum(x1,x2) -> let exp1 = aux x1 in let exp2 = aux x2 in exp1+exp2
	  	|Greater(x1,x2) -> if (aux x1)>(aux x2) then 1 else -1
	   	|Equal(x1,x2) -> if (aux x1)=(aux x2) then 1 else -1
	   	|Less(x1,x2) -> if (aux x1)<(aux x2) then 1 else -1
	   	|Mult(x1,x2) -> let exp1 = aux x1 in let exp2 = aux x2 in exp1*exp2
	   	|Pow(x1,x2) -> let exp1 = aux x1 in let exp2 = aux x2 in (pow exp1 exp2) (* my own pow function *)
	   	|Print(x1) -> let exp = aux x1 in print_int exp;print_string "\n"; 1
	  	|If(x1,x2,x3) -> if (aux x1)=1 then aux x2 else aux x3 
	   	|While(x1,x2) -> if (aux x1)=1 then (aux x2; aux (While(x1,x2))) 
	   		else -1 	
	   	|Paren(x1) -> aux x1
	in aux x; env
;;

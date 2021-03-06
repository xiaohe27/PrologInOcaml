open ProjCommon
open Unify

let rec updateMemory m x v = match m with
				[] -> [(x,v)] |

				(x0,v0)::tail -> if x0=x then (x,v)::tail else (x0,v0)::(updateMemory tail x v)
 ;;

let const_to_val c = match c with 
			BoolConst b -> BoolVal b |
			IntConst i -> IntVal i |
			FloatConst f -> FloatVal f |
			StringConst s -> StringVal s ;;

let rec val2Term value = match value with
			BoolVal b -> ConstTerm (BoolConst b) |
			IntVal i -> ConstTerm (IntConst i) |
			FloatVal f -> ConstTerm (FloatConst f) |
			StringVal s -> ConstTerm (StringConst s) |
			ListVal vl -> ListTerm (List.map (val2Term) vl);;

let monOpApply op v = match op with 
                "not" -> (match v with BoolVal true -> (BoolVal false) |
			 BoolVal false -> (BoolVal true) |
			 _ -> raise (Failure "only support negation of boolean values at current stage.")) |
			
		"-" -> (match v with IntVal i -> IntVal (-i) |
							_ -> raise (Failure "IntNegOp can only operate on integers!")) |
		
		_ -> (raise (Failure "Not supported this op at present"));;




let nlOpApply () = print_newline (); true;;

let writeOpApply contentsTerm =
                 match contentsTerm with
		   ConstTerm(StringConst contents) -> (
                 print_string (contents); true) |
                 _ -> raise (Failure "Write is a built-in operation which takes only one string as argument!");;

let rec binOpApply binop (v1,v2) = match binop with
				"+" -> (match (v1, v2) with 
						(IntVal i1, IntVal i2) -> (IntVal (i1 + i2)) |

						(FloatVal f1, FloatVal f2) -> (FloatVal (f1 +. f2)) |

		(IntVal i1, FloatVal i2) -> (FloatVal (float_of_int i1 +. i2)) |
		(FloatVal i1, IntVal i2) -> (FloatVal (float_of_int i2 +. i1)) |

		(StringVal s1, StringVal s2)->(StringVal (s1 ^ s2)) |

						 _ -> raise (Failure "Unsupported operands!") ) |

				"-" -> (match (v1, v2) with 
						(IntVal i1, IntVal i2) -> (IntVal (i1 - i2)) |
						
						(FloatVal f1, FloatVal f2) -> (FloatVal (f1 -. f2)) |

		(IntVal i1, FloatVal i2) -> (FloatVal (float_of_int i1 -. i2)) |
		(FloatVal i1, IntVal i2) -> (FloatVal (i1 -. float_of_int i2)) |

						 _ -> raise (Failure "Unsupported operands!") ) |

				"*" -> (match (v1, v2) with 
						(IntVal i1, IntVal i2) -> (IntVal (i1 * i2)) |

						(FloatVal f1, FloatVal f2) -> (FloatVal (f1 *. f2)) |

		(IntVal i1, FloatVal i2) -> (FloatVal (float_of_int i1 *. i2)) |
		(FloatVal i1, IntVal i2) -> (FloatVal (float_of_int i2 *. i1)) |

						 _ -> raise (Failure "Unsupported operands!") ) |

				"/" -> (match (v1, v2) with 
						(IntVal i1, IntVal i2) -> (if i2 = 0 then raise (Failure "ERROR: divide by 0") else (IntVal (i1 / i2))) |
						
						(FloatVal f1, FloatVal f2) -> (if f2 = 0.0 then raise (Failure "ERROR: divide by 0") else (FloatVal (f1 /. f2))) |

						(IntVal i1, FloatVal f2) -> (if f2 = 0.0 then raise (Failure "ERROR: divide by 0") else (FloatVal (float_of_int i1 /. f2))) |

						(FloatVal f1, IntVal i2) -> (if i2 = 0 then raise (Failure "ERROR: divide by 0") else (FloatVal (f1 /. float_of_int i2))) |

						_ -> raise (Failure "Both operands should be integers!") ) |

				

								

				"::" -> (match (v1, v2) with
						(_, ListVal vl) -> (ListVal (v1::vl)) |
						_ -> raise (Failure "second argument should be a list!") ) |

				"," -> (match v1 with BoolVal false -> BoolVal false |
					BoolVal true -> (
						match v2 with BoolVal false -> BoolVal false |
					BoolVal true -> (
						BoolVal true
					) |
					_-> raise (Failure ", can only connect boolean values!")	
					) |
					_-> raise (Failure ", can only connect boolean values!") ) |


				";" -> (match v1 with BoolVal true -> BoolVal true |
					BoolVal false -> (
						match v2 with BoolVal false -> BoolVal false |
					BoolVal true -> (
						BoolVal true
					) |
					_-> raise (Failure "; can only connect boolean values!")	
					) |
					_-> raise (Failure "; can only connect boolean values!") ) |


				"=:=" -> (match (v1, v2) with 
						(IntVal i1, IntVal i2) -> (BoolVal (i1 = i2)) |

						(FloatVal f1, FloatVal f2) -> (BoolVal (f1 = f2)) |

		(IntVal i1, FloatVal i2) -> (BoolVal (float_of_int i1 = i2)) |
		(FloatVal i1, IntVal i2) -> (BoolVal (float_of_int i2 = i1)) |
		
		(StringVal s1, StringVal s2) -> (BoolVal (s1 = s2)) |

		(BoolVal b1, BoolVal b2) -> (BoolVal (b1 = b2)) |

						 _ -> raise (Failure "Unsupported operands!") ) |

		       

				">" ->  (match (v1, v2) with 
						(IntVal i1, IntVal i2) -> (BoolVal (i1 > i2)) |

						(FloatVal f1, FloatVal f2) -> (BoolVal (f1 > f2)) |

		(IntVal i1, FloatVal i2) -> (BoolVal (float_of_int i1 > i2)) |
		(FloatVal i1, IntVal i2) -> (BoolVal (float_of_int i2 < i1)) |

		(StringVal s1, StringVal s2)->(BoolVal (s1 > s2)) |

						 _ -> raise (Failure "Unsupported operands!") ) |

				">=" -> (binOpApply ";" ((binOpApply ">" (v1,v2)), (binOpApply "=" (v1,v2) ) )) |

				"<" -> (binOpApply ">" (v2,v1)) |

				"<=" -> (binOpApply ";" ((binOpApply "<" (v1,v2)), (binOpApply "=" (v1,v2) ) )) |

				"=\\=" -> (binOpApply ";" (binOpApply ">" (v1,v2), binOpApply "<" (v1,v2) ) ) |

				"mod" -> (match (v1, v2) with 
						(IntVal i1, IntVal i2) -> (IntVal (i1 mod i2)) |
						_ -> raise (Failure "Both operands should be integers!") ) |

				"**" ->  (match (v1, v2) with 
						(IntVal i1, IntVal i2) -> (FloatVal (float_of_int i1 ** float_of_int i2)) |

						(FloatVal f1, FloatVal f2) -> (FloatVal (f1 ** f2)) |

		(IntVal i1, FloatVal i2) -> (FloatVal (float_of_int i1 ** i2)) |
		(FloatVal i1, IntVal i2) -> (FloatVal (i1 ** float_of_int i2)) |
			_ -> raise (Failure "Unsupported operands!")) | 

				_ -> raise (Failure "Unsupported operations!")  


				;;						


let rec typeTest ty term = match ty with 
                            "var" -> (match term with 
			       Var _ -> true |
			       _ -> false) 

			| "nonvar" -> (match term with 
			       Var _ -> false |
			       _ -> true)

			| "atom" -> (match term with
				ConstTerm(StringConst _) -> true |
				_ -> false)
			
			| "integer" -> (match term with 
				ConstTerm(IntConst _) -> true |
				_ -> false)

			| "float" -> (match term with 
				ConstTerm(FloatConst _) -> true |
				_ -> false)

			| "number" ->  (typeTest "integer" term) ||
					(typeTest "float" term)

			| "atomic" -> (typeTest "atom" term) ||
							(typeTest "number" term)

			| "compound" -> (match term with 
				CompoundTerm(_,_) -> true |
				_ -> false)

			| "callable" -> (typeTest "atom" term) ||
						(typeTest "compound" term)

			| "list" -> (match term with 
				ListTerm _ -> true 
				| _ -> false)

			| "is_list" -> (match term with 
				ListTerm _ -> true 
				| _ -> false)

			| _ -> (print_string "This operation is not supported yet";
				false);;
				


let rec eval_term term = match term with
				ConstTerm(t) -> (const_to_val t) 
			|	Var(x) -> (raise (Failure "Var Not Instantiated yet"))

			|       CompoundTerm(f,tl) -> (if isBuiltInOp f then 
			   (if (isTypeTesting f) then 
					                (if (List.length tl)!=1  then (BoolVal false) else BoolVal(typeTest f (List.hd tl)) )
						 else (     
							             if (List.length tl) != 2 then raise (Failure "number of args to the function is wrong")
									 else (let eq= (List.hd tl, List.nth tl 1) in
									 
							             (match f with 
								     "=" -> (match (Unify.unify [eq]) with  
									     None -> BoolVal(false) |
									     (Some sig0) -> BoolVal(true) ) |

								     "is" -> (let lhs= fst eq in 
									      let rhs= snd eq in
									     let rhsVal = eval_term rhs in
									     
									     match lhs with 
									      ConstTerm _ -> (binOpApply "=:=" (eval_term lhs,rhsVal) ) |
									      Var x -> BoolVal(true) |
									      _ -> BoolVal(false)) |
								     _ -> (raise (Failure "Not supported yet."))) )  
			                                            
			                                ) )   

			else ( if (List.length tl) == 2 then (binOpApply f (eval_term (List.hd tl), eval_term (List.nth tl 1))) else (raise (Failure "Not supported yet")) ) ) 

			|       ListTerm(tl) -> (ListVal (List.map (eval_term) tl))

			|       PredAsTerm _ -> (raise (Failure "should not evaluate a predicate term here!"))

		;;



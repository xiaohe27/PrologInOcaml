open ProjCommon


let rec updateMemory m x v = match m with
				[] -> [(x,v)] |

				(x0,v0)::tail -> if x0=x then (x,v)::tail else (x0,v0)::(updateMemory tail x v)
 ;;

let const_to_val c = match c with 
			BoolConst b -> BoolVal b |
			IntConst i -> IntVal i |
			FloatConst f -> FloatVal f |
			StringConst s -> StringVal s ;;

let val2Term value = match value with
			BoolVal b -> ConstTerm (BoolConst b) |
			IntVal i -> ConstTerm (IntConst i) |
			FloatVal f -> ConstTerm (FloatConst f) |
			StringVal s -> ConstTerm (StringConst s) |
			ListVal vl -> ListTerm (List.map (val2Term) vl);;

let monOpApply op v = match op with 
                "not" -> (match v with BoolVal true -> BoolVal false |
			 BoolVal false -> BoolVal true |
			 _ -> raise (Failure "only support negation of boolean values at current stage.")) |
			
		"-" -> (match v with IntVal i -> IntVal (-i) |
							_ -> raise (Failure "IntNegOp can only operate on integers!")) |
		
		_ -> (raise (Failure "Not supported this op at present"));;


(*Type testing*)
let isTypeTesting op = match op with 
			"var" -> true |
			"nonvar" -> true |
			"atom" -> true |
			"integer" -> true |
			"float" -> true |
			"number" -> true |
			"atomic" -> true |
			"compound" -> true |
			"callable" -> true |
			"list" -> true |
			"is_list" -> true |
			_ -> false;;



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

			| _ -> (raise (Failure "Not supported yet"));;



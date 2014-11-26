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

let monOpApply op v = match op with 
			HdOp -> (match v with ListVal(vl) -> if vl = [] then Exn(0) else  List.hd vl |
					_ -> raise (Failure "Hd Op can only operate on list!") )

		|	TlOp -> (match v with ListVal(vl) -> if vl = [] then Exn(0) else  ListVal(List.tl vl) |
					_ -> raise (Failure "Tl Op can only operate on list!") ) 

		|	PrintOp -> (match v with StringVal s -> print_string s; UnitVal |
							_ -> raise (Failure "PrintOp can only operate on strings!"))

		|	IntNegOp -> (match v with IntVal i -> IntVal (-i) |
							_ -> raise (Failure "IntNegOp can only operate on integers!"))

		| 	FstOp -> (match v with PairVal(v1, v2) -> v1 |
					_ -> raise (Failure "FstOp can only operate on pairs!"))

		|	SndOp -> (match v with PairVal(v1, v2) -> v2 |
					_ -> raise (Failure "SndOp can only operate on pairs!"))

let binOpApply binop (v1,v2) = match binop with
				IntPlusOp -> (match (v1, v2) with 
						(IntVal i1, IntVal i2) -> (IntVal (i1 + i2)) |
						_ -> raise (Failure "Both operands should be integers!") ) |

				IntMinusOp -> (match (v1, v2) with 
						(IntVal i1, IntVal i2) -> (IntVal (i1 - i2)) |
						_ -> raise (Failure "Both operands should be integers!") ) |

				IntTimesOp -> (match (v1, v2) with 
						(IntVal i1, IntVal i2) -> (IntVal (i1 * i2)) |
						_ -> raise (Failure "Both operands should be integers!") ) |

				IntDivOp -> (match (v1, v2) with 
						(IntVal i1, IntVal i2) -> if i2 = 0 then Exn(0) else (IntVal (i1 / i2)) |
						_ -> raise (Failure "Both operands should be integers!") ) |

				FloatPlusOp -> (match (v1, v2) with 
						(FloatVal f1, FloatVal f2) -> (FloatVal (f1 +. f2)) |
						_ -> raise (Failure "Both operands should be float numbers!") ) |

				FloatMinusOp -> (match (v1, v2) with 
						(FloatVal f1, FloatVal f2) -> (FloatVal (f1 -. f2)) |
						_ -> raise (Failure "Both operands should be float numbers!") ) |

				FloatTimesOp -> (match (v1, v2) with 
						(FloatVal f1, FloatVal f2) -> (FloatVal (f1 *. f2)) |
						_ -> raise (Failure "Both operands should be float numbers!") ) |

				FloatDivOp -> (match (v1, v2) with 
						(FloatVal f1, FloatVal f2) -> if f2 = 0.0 then Exn(0) else (FloatVal (f1 /. f2)) |
						_ -> raise (Failure "Both operands should be float numbers!") ) |

				ConcatOp -> (match (v1, v2) with 
						(StringVal s1, StringVal s2) -> (StringVal (s1 ^ s2)) |
						_ -> raise (Failure "Both operands should be strings!") ) |

				ConsOp -> (match (v1, v2) with
						(_, ListVal vl) -> (ListVal (v1::vl)) |
						_ -> raise (Failure "second argument should be a list!") ) |

				CommaOp -> (PairVal(v1, v2)) |

				EqOp -> (BoolVal(v1 = v2)) |

				GreaterOp -> (BoolVal(v1 > v2)) |

				ModOp -> (match (v1, v2) with 
						(IntVal i1, IntVal i2) -> (IntVal (i1 mod i2)) |
						_ -> raise (Failure "Both operands should be integers!") ) |

				ExpoOp -> (match (v1, v2) with 
						(FloatVal f1, FloatVal f2) -> (FloatVal (f1 ** f2)) |
						_ -> raise (Failure "Both operands should be float numbers!") ) 

				;;						

let rec eval_exp (exp, m) = match exp with
				ConstExp(c) -> const_to_val c 
			|	VarExp(x) -> (match (lookup_env m x) with
						Some v -> (match v with 
						  		RecVarVal(g,y,e,m') -> let m''=
							updateMemory m' g (RecVarVal(g,y,e,m')) in Closure(y,e,m'') |

								_ -> v ) |
						None -> (raise (Failure "No mapping for this variable !")) 
						)

			|	MonOpAppExp(mon, e) -> (let v= eval_exp (e,m) in 
			                                    match v with 
							      Exn(i) -> Exn(i) |
							      _ -> monOpApply mon v) 

			|	BinOpAppExp(bin, e1, e2) -> (	let v1=eval_exp(e1,m) in 
			                                         match v1 with Exn(i) -> Exn(i) |
								 _ ->
								let v2=eval_exp(e2,m) in
								 match v2 with Exn(j) -> Exn(j) |
								 _ ->
								binOpApply bin (v1,v2) )

			|	IfExp(e1,e2,e3) -> (match (eval_exp(e1,m)) with
			                                Exn(i) -> Exn(i) |
							BoolVal b -> 
							if b then eval_exp(e2,m) else eval_exp(e3,m) |
							_ -> raise (Failure "conditional expression should be evaluated to boolean value!") )

			|	LetInExp(x,e1,e2) -> (let v1= eval_exp(e1,m) in
			                                 match v1 with Exn(i) -> Exn(i) |
							 _ ->
							let m'= updateMemory m x v1 in
						     eval_exp (e2, m')) 

			|       FunExp(x,e) -> (Closure(x,e,m))

			|       AppExp(e1,e2) -> (match (eval_exp (e1,m)) with 
			                            Exn(i) -> Exn(i) |
						 Closure(x,e',m') -> (let v'= eval_exp (e2,m) in
						                       match v' with Exn(j) -> Exn(j) |
								       _ ->
								      let m''= updateMemory m' x v' in
								     eval_exp (e',m'')) |
						 _ -> (raise (Failure "function expression not well formed.")))

			|       RaiseExp(e) ->( match (eval_exp (e,m)) with
			                         IntVal n -> Exn(n) |
						 Exn(i) -> Exn(i) |
					         _ -> (raise (Failure "expression after raise can only be integer.")))

		        | TryWithExp(e,n1,e1,cl) -> ( match eval_exp(e,m) with
			                               Exn(j) -> (match n1 with 
								 Some i -> (if i=j then eval_exp(e1,m) else
								 if cl=[] then Exn(j) else
								 eval_exp(TryWithExp(e,fst(List.hd cl),snd(List.hd cl),List.tl cl) ,m) ) |
								 None -> eval_exp(e1,m))
			                             | v -> v )

			| LetRecInExp(f,x,e1,e2) -> ( let m'=updateMemory m f (RecVarVal(f,x,e1,m)) in
							eval_exp(e2,m') )

			|	 _ -> raise (Failure "Not implemented yet.")

let eval_dec (dec, m) = match dec with 
				Anon(e) -> ((None, eval_exp (e,m)), m) 
                           |
				Let(x,e) -> (let v= eval_exp (e,m) in 
				               match v with Exn(i) -> ((None,Exn(i)),m) |
					       _ ->
						((Some x,v), updateMemory m x v) )
			   |
			        LetRec(f,x,e) -> ((Some f,RecVarVal(f,x,e,m)), make_env f (RecVarVal(f,x,e,m)))

			  ;;

open Expr
open CS51Utils
open Absbook
open Evaluation
module Ev = Evaluation ;;


let subst_test () =
    unit_test ((subst "x" (Num 5) (Num 5)) = (Num 5))  
                "test of test of substitution 5 in addition to 5"; 

    unit_test ((subst "x" (Num 5) (Bool true)) = (Bool true))  
                "test of test of substitution true in addition to 5";

    unit_test ((subst "x" (Num 5) Raise) = Raise)  
                "test of substitution raise in addition to 5";

    unit_test ((subst "x" (Num 5) Unassigned) = Unassigned)  
                "test of substitution ungassigned in addition to 5";

    unit_test ((subst "x" (Num 5) (Var "x")) = (Num 5))  
                "test of substitution free x in addition to 5 in x";
    unit_test ((subst "y" (Num 5) (Var "x")) = (Var "x"))  
                "test of substitution free y in addition to 5 in x";

    unit_test ((subst "x" (Num 5) (Unop (Negate, Var "x"))) = (Unop (Negate, Num 5)))
                "test of substitution -x in addition to 5";

    unit_test ((subst "x" 
                      (Num 5) 
                      (Binop (Equals, Var "x", Num 5))) 
                    = Binop (Equals, Num 5, Num 5))
                "test of substitution x = 5 in addition to 5";
    unit_test ((subst "x" (
                      Num 5) 
                      (Binop 
                        (Equals, 
                         Var "x", 
                         Binop (Plus, Var "x", Num 1)))) 
                    = Binop (Equals, Num 5, Binop (Plus, Num 5, Num 1)))
                "substition x = x + 1 in addition to 5";
    unit_test (subst "x" 
                      (Num 5)
                      (Let ("y", (Binop (Plus, Var "x", Num 2)), Var "x")) 
                    = ((Let ("y", (Binop (Plus, Num 5, Num 2)), Num 5))))
                "test of substitution let y = x + 2 in x in addition to x = 5";
    unit_test (subst "x" 
                      (Binop (Plus, Var "y", Num 1))
                      (Let ("y", (Binop (Plus, Var "x", Num 2)), Var "x")) 
                    = ((Let ("var1", 
                         (Binop (Plus, (Binop (Plus, Var "y", Num 1)), Num 2)), 
                         (Binop (Plus, Var "y", Num 1))))))
                "test of substitution let y = x + 2 in x in addition to x = y + 1";
    
    unit_test (subst "x" 
                      (Num 5)
                      (Letrec ("x", (Binop (Plus, Var "x", Num 2)), Var "x")) 
                    = ((Letrec ("x", (Binop (Plus, Num 5, Num 2)), Var "x"))))
                "test of substitution let rec x = x + 2 in x in addition to x = 5";
    unit_test (subst "x" 
                      (Num 5)
                      (Letrec ("y", (Binop (Plus, Var "x", Num 2)), Var "x")) 
                     = ((Letrec ("y", (Binop (Plus, Num 5, Num 2)), Num 5))))
                "test of substitution let rec y = x + 2 in x in addition to x = 5";
    unit_test (subst "x" 
                      (Binop (Plus, Var "y", Num 1))
                      (Letrec ("y", (Binop (Plus, Var "x", Num 2)), Var "x")) 
                    = ((Letrec ("var2", 
                            (Binop (Plus, (Binop (Plus, Var "y", Num 1)), Num 2)), 
                            (Binop (Plus, Var "y", Num 1))))))
                "test of substitution let rec y = x + 2 in x in addition to x = y + 1";

    unit_test (subst "x" (Num 5) (App ((Fun ("x", Binop (Plus, Var "x", Num 2))), Num 1)) 
                    = (App ((Fun ("x", Binop (Plus, Var "x", Num 2))), Num 1)))
                "test of substitution (fun x -> x + 2) 1 in addition to x = 5";
    unit_test (subst "x" (Num 5) (App ((Fun ("y", Binop (Plus, Var "x", Num 2))), Var "x")) 
                    = (App ((Fun ("y", Binop (Plus, Num 5, Num 2))), Num 5)))
                "test of substitution (fun y -> x + 2) x in addition to x = 5" ;;

let test_evald () = 
    let x6 = new_varname () in
    let x7 = new_varname () in
    let x8 = new_varname () in

    print_endline x6 ;
    print_endline x7 ;
    print_endline x8 ;

    assert(eval_d (Let (x6, Fun (x7, Binop (Plus, Var x7, Num 1)), App (Var x6, Num 2))) (Env.empty ()) = Env.Val (Num 3)) ;
	assert(eval_d (Let (x6, Num 3, Let (x6, Num 5, Binop (Plus, Var x6,Var x6)))) (Env.empty ()) = Env.Val (Num 10)) ;
    assert(eval_d (App (Fun (x6, Binop(Plus, Var x6, Var x6)), Binop(Times, Num 1, Num 2))) (Env.empty ()) = Env.Val (Num 4)) ;
    assert(eval_d (Let (x6, Num 1, Let (x7, Num 2, Binop (Plus, Var x6,Var x7)))) (Env.empty ()) = Env.Val (Num 3)) ;
    assert(eval_d (Letrec (x6, Fun (x7, Conditional(Binop(Equals,Var x7, Num 0), Num 1, Binop(Times, Var x7, App(Var x6, Binop(Minus, Var x7, Num 1))))), App (Var x6, Num 4))) (Env.empty ()) = Env.Val (Num 24))
    ;;

let test_freevars () = 
    
    let x1 = new_varname () in
    let x2 = new_varname () in
    let x3 = new_varname () in

	
    assert(free_vars (Var x1) = vars_of_list [x1] );
    assert(free_vars (Num 0) =  vars_of_list []);
    assert(free_vars Raise =  vars_of_list [] );
    assert(free_vars Unassigned =  vars_of_list [] );
    assert(free_vars (Bool true) =  vars_of_list [] );
    assert(free_vars (Unop (Negate, Var x1)) =  vars_of_list [x1]);
    assert(free_vars (Conditional (Var x1, Var x2, Var x3)) =  vars_of_list [x1;x2;x3] );
    assert(free_vars (Fun (x1, Var x2)) =  vars_of_list [x2]);
    assert(free_vars (Fun (x1, Binop (Plus, Var x1, Var x2))) =  vars_of_list [x2]);
    assert(free_vars (Let (x1, (Binop (Plus, Var x1, Var x3)), Var x2)) =  vars_of_list [x1;x3;x2]) ;
    assert(free_vars (Letrec (x1, Var x2, Var x3)) = vars_of_list [x2;x3]) ;
    assert(free_vars Raise =  vars_of_list [] );
    assert(free_vars Unassigned =  vars_of_list [] );
    assert(free_vars (App (Var x1, Var x2)) =  vars_of_list [x1;x2] )



(*evaluate all*)
let _ =
  print_newline ();
  test_freevars () ;
  test_evald () ;
  subst_test () ;
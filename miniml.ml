(* 
                         Source Code by Sid Bharthulwar  
                    Minicaml -- Read-Eval-Print Loop
 *)

module Ev = Evaluation ;;
module MP = Minicaml_parse ;;
module ML = Minicaml_lex ;;
module Ex = Expr ;;

open Printf ;;

(* str_to_exp str -- Returns the expression specified by `str` using
   the Minicaml parser. *)
let str_to_exp (str: string) : Ex.expr =
  let lexbuf = Lexing.from_string str in
  let exp = MP.input ML.token lexbuf in
  exp ;;

(* repl () -- Read-eval-print loop for Minicaml, which prompts for and
   evaluates Minicaml expressions, printing the resulting value. Exits
   the loop and terminates upon reading an end-of-file
   (control-d). *)
let repl () =
  (* lexical analyzer buffer from stdin *)
  let lexbuf = Lexing.from_channel stdin in
  (* set up the initial environment *)
  let env = Ev.Env.empty () in

  (* the main LOOP *)
  while true do
    (try
        (* prompt *)
        printf "<== %!";
        
        (* READ and parse an expression from the input *)
        let exp = MP.input ML.token lexbuf in 
        
        (* EVALuate it *)
        let res = Ev.evaluate exp env in
         
        (* PRINT the result; in this initial version, the trivial
           evaluator just returns the expression unchanged as an
           element of the `Env.value` type (found in `expr.ml`), so we
           just extract the expression back out and print it *)
        match res with
        | Val resexp ->
           printf "==> %s\n" (Ex.exp_to_concrete_string resexp)
        | _ -> failwith "not handling other cases yet"
      with
      | MP.Error -> printf "xx> parse error\n"
      | Ev.EvalError msg -> printf "xx> evaluation error: %s\n" msg
      | Ev.EvalException -> printf "xx> evaluation exception\n"
      | End_of_file -> printf "Goodbye.\n"; exit 0
    );
    flush stdout
  done
;;
        
(* Run REPL if called from command line *)

try
  let _ = Str.search_forward (Str.regexp "Minicaml\\.\\(byte\\|native\\|bc\\|exe\\)")
                             (Sys.argv.(0)) 0 in
  repl ()
with Not_found -> () ;;

(*  Concrete syntax
e :: x | n | true | false | succ | pred | iszero | if e then e else e
       | fn x => e | e e | (e) | let x = e in e

Abstract Syntax
datatype term = AST_ID of string | AST_NUM of int | AST_BOOL of bool
  | AST_SUCC | AST_PRED | AST_ISZERO | AST_IF of (term * term * term)
  | AST_FUN of (string * term) | AST_APP of (term * term)
  | AST_LET of (string * term * term)
  | AST_ERROR of string
*)

use "parser.sml";

datatype result = RES_ERROR of string | RES_NUM of int| RES_BOOL  of bool
                | RES_SUCC | RES_PRED | RES_ISZERO | RES_FUN of (string * term)

exception UnboundID
datatype env = Env of (string -> term)
fun emptyenvFun  (x : string) : term = raise UnboundID;
val emptyenv = Env emptyenvFun
fun update (Env e) (x : string) (ty : term) = fn y => if x = y then ty else e y

exception Not_implemented_yet
exception Error of string


fun interp (env, AST_ID i)          = raise Not_implemented_yet 
  | interp (env, AST_NUM n)         = RES_NUM n
  | interp (env, AST_BOOL b)        = RES_BOOL b
  | interp (env, AST_FUN (i,e))     = RES_FUN (i,e)
  | interp (env, AST_APP (e1,e2))   = (case interp (env, e1) of
         RES_FUN (x, body) => raise Not_implemented_yet 
       | RES_SUCC =>  raise Not_implemented_yet
       | RES_PRED =>  raise Not_implemented_yet
       | RES_ISZERO => raise Not_implemented_yet
       | _ => raise Error "apply non-function")
  | interp (env, AST_SUCC)          = RES_SUCC
  | interp (env, AST_PRED)          = RES_PRED
  | interp (env, AST_ISZERO)        = RES_ISZERO
  | interp (env, AST_IF (e1,e2,e3)) =  (case interp (env,e1) of
                                     RES_BOOL true  => interp (env,e2)
                                   | RES_BOOL false => interp (env,e3)
                                   | _              => raise Error  "case on non-bool")

  | interp (env, AST_LET (x,e1,e2)) = raise Not_implemented_yet
  | interp (env, AST_ERROR s)       = raise Error s

fun eval s = interp (emptyenv, parsestr s);

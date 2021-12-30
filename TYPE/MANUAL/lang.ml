type program = exp
and exp =
    | CONST of int
    | VAR of var
    | ADD of exp * exp
    | SUB of exp * exp
    | ISZERO of exp
    | IF of exp * exp * exp
    | LET of var * exp * exp                            
    | PROC of var * ttype * exp                        (*proc (x:t) E*)
    | LETREC of ttype* var * var * ttype * exp * exp   (*letrec t1 f(x:t2) = E in E *)
    | APPLY of exp * exp
and var = string
and ttype = 
  | TInt
  | TBool
  | TProc of ttype * ttype
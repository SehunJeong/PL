type exp =
    | CONST of int
    | VAR of var
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | READ
    | ISZERO of exp
    | IF of exp * exp * exp
    | LET of var * exp * exp                            
    | LETREC of var * var * exp * exp
    | PROC of var * exp
    | CALL of exp * exp
and var = string

type typ = TyInt | TyBool | TyFun of typ * typ | TyVar of tyvar 
and tyvar = string
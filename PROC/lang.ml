type program = exp
and exp =
    | CONST of int
    | VAR of var
    | ADD of exp * exp
    | SUB of exp * exp
    | READ
    | ISZERO of exp
    | IF of exp * exp * exp
    | LET of var * exp * exp
    | LETREC of var * var * exp * exp
    | LETMREC of var * var * exp * var * var * exp * exp
    | PROC of var * exp
    | SEQ of exp * exp
and var = string

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
and var = string

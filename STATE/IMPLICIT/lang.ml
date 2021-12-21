type program = exp
and exp =
    | CONST of int
    | VAR of var
    | ADD of exp * exp
    | SUB of exp * exp
    | ISZERO of exp
    | IF of exp * exp * exp
    | LET of var * exp * exp
    | PROC of var * exp
    | APPLY of exp * exp
    | ASSIGN of var * exp
    | SEQ of exp * exp
and var = string

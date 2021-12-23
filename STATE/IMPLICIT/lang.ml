type program = exp
and exp =
    | CONST of int
    | VAR of var
    | ADD of exp * exp
    | SUB of exp * exp
    | ISZERO of exp
    | IF of exp * exp * exp
    | LET of var * exp * exp
    | LETREC of var * var * exp * exp
    | PROC of var * exp
    | APPLY of exp * exp
    | APPLYREF of exp * var
    | ASSIGN of var * exp
    | SEQ of exp * exp
    | EMPTYREC
    | RECORD of var * exp * var * exp
    | FLD of exp * var
    | ASSIGNFLD of exp * var * exp
    | DEREF of var
    | DEREFFLD of exp * var
    | REF of exp
    | ASSIGNREF of exp * exp
and var = string

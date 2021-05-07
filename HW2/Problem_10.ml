type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp;;

let calculator: exp -> int = fun e ->
    0;;

let example = SIGMA(INT 1, INT 10, SUB (MUL (X, X), INT 1));;

print_endline(string_of_int(calculator example));;

exception InvalidInputException of string;;

type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp;;

let rec replace: exp -> int -> exp = fun e i ->
  match e with
  | X -> INT i
  | INT i' -> e
  | ADD (e1, e2) -> ADD (replace e1 i, replace e2 i)
  | SUB (e1, e2) -> SUB (replace e1 i, replace e2 i)
  | MUL (e1, e2) -> MUL (replace e1 i, replace e2 i)
  | DIV (e1, e2) -> DIV (replace e1 i, replace e2 i)
  | SIGMA (e1, e2, e3) -> SIGMA (replace e1 i, replace e2 i, e3);;

let rec calculator: exp -> int = fun e ->
  match e with
  | X -> raise(InvalidInputException "Invalid Input!")
  | INT i -> i
  | ADD (e1, e2) -> (calculator e1) + (calculator e2)
  | SUB (e1, e2) -> (calculator e1) - (calculator e2)
  | MUL (e1, e2) -> (calculator e1) * (calculator e2)
  | DIV (e1, e2) -> (calculator e1) / (calculator e2)
  | SIGMA (e1, e2, e3) -> begin 
      let int_of_e1 = calculator e1 in
      let int_of_e2 = calculator e2 in 
      assert(int_of_e1 <= int_of_e2);
      
      if int_of_e1 = int_of_e2 then calculator (replace e3 int_of_e1)
      else (calculator (replace e3 int_of_e1)) + (calculator (SIGMA (ADD (e1, INT 1), e2, e3)))
    end;;

let example1 = SIGMA(INT 1, INT 10, SUB (MUL (X, X), INT 1));;

print_endline(string_of_int(calculator example1));;

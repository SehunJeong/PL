type exp =
| NUM of int | TRUE | FALSE | UNIT
| VAR of id
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| EQUAL of exp * exp
| LESS of exp * exp
| NOT of exp
| SEQ of exp * exp (* sequence *)
| IF of exp * exp * exp (* if-then-else *)
| WHILE of exp * exp (* while loop *)
| LETV of id * exp * exp (* variable binding *)
| LETF of id * id list * exp * exp (* procedure binding *)
| CALLV of id * exp list (* call by value *)
| CALLR of id * id list (* call by referenece *)
| RECORD of (id * exp) list (* record construction *)
| FIELD of exp * id (* access record field *)
| ASSIGN of id * exp (* assgin to variable *)
| ASSIGNF of exp * id * exp (* assign to record field *)
| WRITE of exp
and id = string
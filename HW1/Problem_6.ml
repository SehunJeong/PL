type nat = ZERO | SUCC of nat;;
let rec natadd : nat -> nat -> nat = fun x y ->
  match y with
  | ZERO -> x
  | SUCC z -> natadd (SUCC x) z;;
let rec natmul : nat -> nat -> nat = fun x y ->
  match y with
  | ZERO -> ZERO
  | SUCC ZERO -> x
  | SUCC z -> natadd x (natmul x z);;

let rec nat_to_int : nat -> int = fun x ->
  match x with
  | ZERO -> 0
  | SUCC y -> (nat_to_int y) + 1;;

let two = SUCC(SUCC ZERO);;
let three = SUCC(SUCC(SUCC ZERO));;

print_endline (string_of_int(nat_to_int(natadd two three)));;
print_endline (string_of_int(nat_to_int(natmul two three)));;
print_endline (string_of_int(nat_to_int(natmul two ZERO)));;
print_endline (string_of_int(nat_to_int(natmul ZERO two)));;

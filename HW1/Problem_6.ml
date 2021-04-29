type nat = ZERO | SUCC of nat;;
let natadd : nat -> nat -> nat = fun x y ->
    ZERO;;
let natmul : nat -> nat -> nat = fun x y ->
    ZERO;;

let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;
natmul two three;;

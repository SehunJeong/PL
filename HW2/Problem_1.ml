let rec helper: int -> int -> int = fun i x ->
  
  0;;

let smallest_divisor: int -> int = fun x ->
  if x = 1 || x = 2 then x 
  else helper 1 x;;

smallest_divisor 15;; (*3*)
smallest_divisor 121;; (*11*)
smallest_divisor 141;; (*3*)
smallest_divisor 199;; (*199*)

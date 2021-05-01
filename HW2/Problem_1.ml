(*https://www.geeksforgeeks.org/smallest-prime-divisor-of-a-number/*)

let rec sqrt: int -> int -> int = fun i x ->
  if (i * i) > x then i-1 else sqrt (i+1) x;;

let rec sd_helper: int -> int -> int -> int = fun i sqrt x ->
  if (i*i) > x then -1
  else if (x mod i) = 0 then i
  else sd_helper (i+2) sqrt x;;
  

let smallest_divisor: int -> int = fun x ->
  if x = 1 || x = 2 then x 
  else (
    let s = sqrt 0 x in
    let sd = sd_helper 3 s x in
    if sd = -1 then x else sd
  );;

print_endline (string_of_int(smallest_divisor 15));; (*3*)
print_endline (string_of_int(smallest_divisor 121));; (*11*)
print_endline (string_of_int(smallest_divisor 141));; (*3*)
print_endline (string_of_int(smallest_divisor 199));; (*199*)

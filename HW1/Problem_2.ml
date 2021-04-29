let rec prime_helper : int * int -> bool = fun (x, y) ->
  if x <= 2 then (if x = 2 then true else false)
  else if (x mod y = 0) then false
  else if (y * y > x) then true
  else prime_helper(x, y + 1);; 

let prime : int -> bool = fun x -> 
  prime_helper (x, 2);;

print_endline (string_of_bool(prime 2));;
print_endline (string_of_bool(prime 3));;
print_endline (string_of_bool(prime 4));;
print_endline (string_of_bool(prime 17));;

let rec sigma: (int -> int) -> int -> int -> int = fun f a b ->
  assert(a <= b);
  if a = b then f a
  else (f a) + (sigma f (a+1) b);;

print_endline(string_of_int(sigma (fun x -> x) 1 10));;
print_endline(string_of_int(sigma (fun x -> x*x) 1 7));;

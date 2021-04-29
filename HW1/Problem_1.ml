let rec pascal : int * int -> int = fun (x,  y) -> 
  if x = 0 || y = 0 || y = x then 1
  else pascal (y - 1, x - 1) + pascal (y - 1, x);;

print_endline (string_of_int(pascal (0, 0)));
print_endline (string_of_int(pascal (1, 0)));
print_endline (string_of_int(pascal (1, 1)));
print_endline (string_of_int(pascal (2, 1)));
print_endline (string_of_int(pascal (4, 2)));

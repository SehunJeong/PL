let rec dfact : int -> int = fun x ->
    if (x = 2) then 2
    else if (x = 1) then 1
    else x * dfact(x - 2);;

print_endline (string_of_int(dfact 7));;
print_endline (string_of_int(dfact 6));;

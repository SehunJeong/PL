let rec reduce: ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c = fun f l1 l2 i ->
    assert((List.compare_lengths l1 l2) = 0);
    assert((List.length l1 > 0) && (List.length l2 > 0));
    match l1, l2 with
    | [], l2' -> 0
    | l1', [] -> 0
    | [x], [y] -> f x y i
    | x::l1', y::l2' -> reduce f l1' l2' (f x y i);;

print_endline(string_of_int(reduce (fun x y z -> x * y + z) [1;2;3] [0;1;2] 0));;
print_endline(string_of_int(reduce (fun x y z -> x * y + z) [] [0;1;2] 0));;


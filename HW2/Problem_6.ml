let reduce: ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c = fun f l1 l2 i ->
    i;;

reduce (fun x y z -> x * y + z) [1;2;3] [0;1;2] 0;;


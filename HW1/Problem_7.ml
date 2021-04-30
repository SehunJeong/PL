type btree = 
    Empty
  | Node of int * btree * btree;;

let t1 = Node (1, Empty, Empty);;
let t2 = Node (1, Node (2, Empty, Empty), Node (3, Empty, Empty));;

let rec mem : int -> btree -> bool = fun n t ->
  match t with
  | Empty -> false
  | Node (x, left, right) -> 
      if x = n then true else (mem n left) || (mem n right);;

print_endline(string_of_bool(mem 1 t2));;
print_endline(string_of_bool(mem 4 t2));;
print_endline(string_of_bool(mem 4 Empty));;

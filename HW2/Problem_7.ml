type graph = (vertex * vertex) list
and vertex = int;;

let rec step: graph -> vertex list -> vertex list = fun g visited ->
  let append: vertex -> vertex list -> vertex list = fun v vl ->
    if List.mem v vl then vl
    else v::vl
  in
  match g with
  | [] -> visited
  | [(v1, v2)] -> if List.mem v1 visited then append v2 visited else visited
  | (v1, v2)::g' -> if List.mem v1 visited then step g' (append v2 visited) else step g' visited;;

let compare_vertex_list: vertex list -> vertex list -> bool = fun vl1 vl2 ->
  if (List.length vl1) != (List.length vl2) then false
  else
    let vl1_sorted = List.sort compare vl1 in
    let vl2_sorted = List.sort compare vl2 in
    List.fold_right2 (fun v1 v2 b -> (v1 = v2) && b) vl1_sorted vl2_sorted true;;

let rec fix: graph -> vertex list -> vertex list = fun g visited ->
  let after = step g visited in
  if  compare_vertex_list visited after then visited
  else fix g after;;

let reach: graph * vertex -> vertex list = fun (g, v) ->
  List.sort compare (fix g [v]);;

reach ([(1,2);(2,3);(3,4);(4,2);(2,5)], 1);;
reach ([(1,2);(2,3);(3,4);(4,2);(2,5)], 2);;
reach ([(1,2);(2,3);(3,4);(4,2);(2,5)], 3);;
reach ([(1,2);(2,3);(3,4);(4,2);(2,5)], 4);; 
reach ([(1,2);(2,3);(3,4);(4,2);(2,5)], 5);; 

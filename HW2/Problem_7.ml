type graph = (vertex * vertex) list
and vertex = int;;

let reach: graph * vertex -> vertex list = fun (g, v) ->
    let rec step: graph -> vertex list -> vertex list = fun g' r_old ->
        match g' with
        | [] -> r_old
        | [(v1, v2)] -> if List.mem v1 r_old then v2::r_old else r_old
        | (v1, v2)::g'' -> if List.mem v1 r_old then step g'' (v2::r_old) else step g'' r_old
    in
    [v];;

reach ([(1,2);(2,3);(3,4);(4,2);(2,5)], 1);;
reach ([(1,2);(2,3);(3,4);(4,2);(2,5)], 2);;
reach ([(1,2);(2,3);(3,4);(4,2);(2,5)], 3);;
reach ([(1,2);(2,3);(3,4);(4,2);(2,5)], 4);;
reach ([(1,2);(2,3);(3,4);(4,2);(2,5)], 5);;

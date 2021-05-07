type mobile = branch * branch
and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
and length = int
and weight = int;;

let rec weight_of: mobile -> weight = fun m ->
    match m with
    | SimpleBranch (_, w1), SimpleBranch (_, w2) -> w1 + w2
    | SimpleBranch (_, w1), CompoundBranch (_, m2) -> w1 + (weight_of m2)
    | CompoundBranch (_, m1), SimpleBranch (_, w2) -> (weight_of m1) + w2
    | CompoundBranch (_, m1), CompoundBranch (_, m2) -> (weight_of m1) + (weight_of m2);;

let rec balanced: mobile -> bool = fun m ->
    match m with
    | SimpleBranch (l1, w1), SimpleBranch (l2, w2) -> (l1 * w2) = (l2 * w2)
    | SimpleBranch (l1, w1), CompoundBranch (l2, m2) -> (balanced m2) && (l1 * w1) = (l2 * (weight_of m2))
    | CompoundBranch (l1, m1), SimpleBranch (l2, w2) -> (balanced m1) && (l1 * (weight_of m1)) = (l2 * w2)
    | CompoundBranch (l1, m1), CompoundBranch (l2, m2) -> (balanced m1) && (balanced m2) && ((weight_of m1) = (weight_of m2));;

let m = (CompoundBranch (3,
          (CompoundBranch (2, (SimpleBranch (1, 1), SimpleBranch (1, 1))),
           SimpleBranch (1, 4))),
        SimpleBranch (6, 3));;

print_endline (string_of_bool(balanced m));;

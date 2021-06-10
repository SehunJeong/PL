type value = 
  | Int of int
  | Bool of bool

let string_of_value: value -> string
= fun v ->
  match v with
  | Int i -> string_of_int i
  | Bool true -> "true"
  | Bool false -> "false"

let rec tail = function
    | [] -> None
    | [x] -> Some(x)
    | _ :: t -> tail t

let rec last_two = function
    | [] | [_] -> None
    | [x1; x2] -> Some(x1, x2)
    | _ :: t -> last_two t

let rec nth_elem n = function
    | [] -> None
    | h :: t -> if n == 0 then Some(h) else nth_elem (n - 1) t

let rec len = function
    | [] -> 0
    | _ :: t -> 1 + len t

let len_tail_recursive li =
    let rec aux n = function
        | [] -> n
        | _ :: t -> aux (n + 1) t
    in
    aux 0 li

let () = print_int (len_tail_recursive [1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9;0;1;2;3;4;5;6;7;8;9;0])
let () = print_newline ()

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

let reverse li =
    let rec aux acc = function
        | [] -> acc
        | x :: xs -> aux (x :: acc) xs
    in
    aux [] li

let is_palindrome li = li = reverse li

type 'a node =
  | One of 'a 
  | Many of 'a node list

let flatten li =
    let rec aux acc = function
        | [] -> acc
        | x :: xs -> match x with
            | One(a) -> aux (acc @ [a]) xs
            | Many(l) -> aux (acc @ aux [] l) xs
    in
    aux [] li

let flatten2 list =
    let rec aux acc = function
      | [] -> acc
      | One x :: t -> aux (x :: acc) t
      | Many l :: t -> aux (aux acc l) t
    in
    List.rev (aux [] list)

let rec compress = function
    | x :: (y :: _ as t) -> if x = y then compress t else x :: compress t
    | other -> other

let pack li =
    let rec aux curr acc = function
        | [] -> []
        | [x] -> (x :: curr) :: acc
        | a :: (b :: _ as t) ->
                if a = b
                then aux (a :: curr) acc t
                else aux [] ((a :: curr) :: acc) t
    in
    reverse (aux [] [] li)

let flattened = flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]]
let () = List.iter (Printf.printf "%s ") flattened
let () = print_newline ()

let packed = pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"]
let () = List.iter (List.iter (Printf.printf "%s ")) packed
let () = print_newline ()

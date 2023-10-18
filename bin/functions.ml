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

let run_length_encoding li =
    let rec aux n acc = function
        | [] -> []
        | [x] -> (x, n) :: acc
        | a :: (b :: _ as t) ->
                if a = b
                then aux (n + 1) acc t
                else aux 1 ((a, n) :: acc) t
    in
    reverse (aux 1 [] li)

let run_length_encoding_2 li = List.map (fun l1 -> (List.hd l1, len_tail_recursive l1)) (pack li)

type 'a rle =
  | One2 of 'a
  | Many2 of 'a * int

let encode_rle li =
    let create_tuple ch = function
        | 1 -> One2(ch)
        | n -> Many2(ch, n)
    in
    let rec aux n acc = function
        | [] -> []
        | [x] -> create_tuple x n :: acc
        | a :: (b :: _ as t) ->
                if a = b
                then aux (n + 1) acc t
                else aux 1 (create_tuple a n :: acc) t
    in
    reverse (aux 1 [] li)

let decode_rle li =
    let rec n_append c acc = function
        | 0 -> acc
        | x -> n_append c (c :: acc) (x - 1)
    in
    let rec aux acc = function
        | [] -> acc
        | h :: t ->
                match h with
                | One2(x) -> aux (x :: acc) t
                | Many2(c, n) -> aux (n_append c acc n) t
    in
    reverse (aux [] li)

let duplicate li =
    let rec aux acc = function
        | [] -> acc
        | h :: t -> aux (h :: h :: acc) t
    in
    aux [] li

let duplicate_n li n =
    let rec repeat_n h acc i =
        if i = n
        then acc
        else repeat_n h (h :: acc) (i + 1)
    in
    let rec aux acc = function
        | [] -> acc
        | h :: t -> aux (repeat_n h acc 0) t
    in
    aux [] (reverse li)

let drop_n li n =
    let rec aux i = function
        | [] -> []
        | h :: t -> if i == 1 then aux n t else h :: aux (i - 1) t
    in
    aux n li

let split li n =
    let rec aux acc i = function
        | [] -> (reverse acc, [])
        | h :: t ->
                if i = 1
                then (reverse (h :: acc), t)
                else aux (h :: acc) (i - 1) t
    in
    aux [] n li

let slice li i k =
    let rec aux c = function
        | [] -> []
        | _ :: t -> 
                if c = 1
                then
                    let (l1, _) = split t (k - i + 1)
                    in
                    l1
                else aux (c - 1) t
    in
    aux i li

let rotate_n li n =
    let length = len_tail_recursive li in
    let n_ = n mod length in
    let (l1, l2) = split li (length - n_) in
    l2 @ l1

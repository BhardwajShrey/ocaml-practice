let l1 = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]

let () = print_string "Printing list l1..."
let () = List.iter (Printf.printf "%s, ") l1
let () = print_newline ()

let encoded = Functions.run_length_encoding l1
let () = print_string "Printing encoded..."
let () = List.iter (
    fun (ch, freq) ->
        Printf.printf "(%s, %d), " ch freq
) encoded
let () = print_newline ()

(* let flattened = Functions.flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]] *)
(* let () = print_string "Printing flattened..." *)
(* let () = List.iter (Printf.printf "%s ") flattened *)
(* let () = print_newline () *)
(**)
(* let packed = Functions.pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"] *)
(* let () = print_string "Printing packed..." *)
(* let () = List.iter (List.iter (Printf.printf "%s ")) packed *)
(* let () = print_newline () *)

let rle_encoded = Functions.encode_rle l1
let () = print_string "Printing rle_encoded..."
let () = List.iter (
    fun item -> match item with
        | Functions.One2(x) -> Printf.printf "One of %s, " x
        | Functions.Many2(x, n) -> Printf.printf "Many of %s-%d, " x n
) rle_encoded
let () = print_newline ()

let org = Functions.decode_rle rle_encoded
let () = print_string "Reversing rle_encoding..."
let () = List.iter (Printf.printf "%s, ") org
let () = print_newline ()

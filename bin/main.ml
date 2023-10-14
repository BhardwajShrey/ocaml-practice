let l1 = Functions.run_length_encoding ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
let () = print_string "Printing encoded..."
let () = List.iter (
    fun (ch, freq) ->
        Printf.printf "(%s, %d), " ch freq
) l1
let () = print_newline ()

let flattened = Functions.flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]]
let () = print_string "Printing flattened..."
let () = List.iter (Printf.printf "%s ") flattened
let () = print_newline ()

let packed = Functions.pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"]
let () = print_string "Printing packed..."
let () = List.iter (List.iter (Printf.printf "%s ")) packed
let () = print_newline ()

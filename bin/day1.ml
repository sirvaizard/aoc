open Reader

let get_numbers characters =
    let rec get_numbers_aux acc = function
        | [] -> acc
        | h :: t ->
            match h with
            | '0'..'9' -> get_numbers_aux (h :: acc) t
            | _ -> get_numbers_aux acc t
    in get_numbers_aux [] characters

let format_answer numbers = Printf.sprintf "%c%c" (List.hd (List.rev numbers)) (List.hd numbers)

let get_answer line = 
    line |> String.to_seq |> List.of_seq |> get_numbers |> format_answer |> int_of_string 

let () = List.map get_answer (read_file "inputs/day1") |> List.fold_left ( + ) 0 |> print_int

let replace_letters str =
    let rec aux str acc =
        match str with
        | _ when String.length str = 0 -> acc
        | _ when String.starts_with str ~prefix:"one" ->
            aux (String.sub str 1 ((String.length str)-1)) (acc ^ "1")
        | _ when String.starts_with str ~prefix:"two" ->
            aux (String.sub str 1 ((String.length str)-1)) (acc ^ "2")
        | _ when String.starts_with str ~prefix:"three" ->
            aux (String.sub str 1 ((String.length str)-1)) (acc ^ "3")
        | _ when String.starts_with str ~prefix:"four" ->
            aux (String.sub str 1 ((String.length str)-1)) (acc ^ "4")
        | _ when String.starts_with str ~prefix:"five" ->
            aux (String.sub str 1 ((String.length str)-1)) (acc ^ "5")
        | _ when String.starts_with str ~prefix:"six" ->
            aux (String.sub str 1 ((String.length str)-1)) (acc ^ "6")
        | _ when String.starts_with str ~prefix:"seven" ->
            aux (String.sub str 1 ((String.length str)-1)) (acc ^ "7")
        | _ when String.starts_with str ~prefix:"eight" ->
            aux (String.sub str 1 ((String.length str)-1)) (acc ^ "8")
        | _ when String.starts_with str ~prefix:"nine" ->
            aux (String.sub str 1 ((String.length str)-1)) (acc ^ "9")
        | _ -> aux (String.sub str 1 ((String.length str)-1)) (acc ^ (String.sub str 0 1))
    in aux str ""

let get_answer_pt2 line =
    line
    |> replace_letters
    |> String.to_seq
    |> List.of_seq 
    |> get_numbers
    |> format_answer
    |> int_of_string

let () =
    List.map get_answer_pt2 (read_file "inputs/day1")
    |> List.fold_left ( + ) 0
    |> Printf.sprintf "\n%d"
    |> print_endline
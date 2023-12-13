let read_lines channel =
    let rec read_lines_aux acc =
        try
            let line = input_line channel in
            read_lines_aux (line :: acc)
        with End_of_file ->
            List.rev acc
    in read_lines_aux []

let read_file path = 
    let ic = open_in path in
    try
       let lines = read_lines ic in
       close_in ic;
       lines
    with e ->
        close_in ic;
        raise e

let write_file path lines = 
    let oc = open_out path in
    let rec write_lines = function
    | [] -> close_out oc
    | h :: t -> Printf.fprintf oc "%s\n" h; write_lines t
in write_lines lines
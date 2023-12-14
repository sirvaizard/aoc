open Reader

type cube = 
    | Red of int
    | Green of int 
    | Blue of int
type round = { cubes: cube list }
type game = { id: int; rounds: round list}

let lines = read_file "inputs/day2"

let build_round cubes_strs =
    let build_cube entry =
        match String.split_on_char ' ' (String.trim entry) with
        | amount :: color :: _ ->
        begin
            match amount, color with
            | amount, "red" -> Red (int_of_string amount)
            | amount, "green" -> Green (int_of_string amount)
            | amount, "blue" -> Blue (int_of_string amount)
            | _, _ -> failwith "Invalid color"
        end
        | _ -> failwith "Invalid format"
    in { cubes=List.map build_cube cubes_strs}

let build_rounds str =
    String.split_on_char ';' str
    |> List.map (String.split_on_char ',')
    |> List.map build_round 

let get_game_id str =
    match String.split_on_char ' ' str with
    | _ :: number :: _ -> int_of_string number
    | _ -> failwith "Invalid format"

let build_game str rounds = {
    id=get_game_id str;
    rounds=rounds
}

let convert_line_to_game line =
    match String.split_on_char ':' line with
    | game :: rounds :: _ -> build_game game (build_rounds rounds)
    | _ -> failwith "Invalid format"

(* let print_game game =
    let print_round round =
        let print_cube = function
        | Red amount -> print_string (Printf.sprintf " red: %d" amount)
        | Green amount -> print_string (Printf.sprintf " green: %d" amount)
        | Blue amount -> print_string (Printf.sprintf " blue: %d" amount)
    in List.iter (fun cube -> print_cube cube; print_string "; ") round.cubes;
    print_endline ""
in print_endline (Printf.sprintf "game: %d" game.id);
    List.iter (fun round -> print_round round) game.rounds;
    print_endline "" *)

let is_valid_game game =
    let is_valid_round round =
        let is_valid_cube = function
        | Red amount -> amount <= 12
        | Green amount -> amount <= 13
        | Blue amount -> amount <= 14
    in List.for_all (fun cube -> is_valid_cube cube) round.cubes;
in List.for_all (fun round -> is_valid_round round) game.rounds

let max_cubes_per_rounds rounds =
    let rec aux cubes red green blue =
        match cubes with
        | [] -> red, green, blue
        | h :: t -> begin
            match h with
            | Red amount -> aux t (max amount red) green blue
            | Green amount -> aux t red (max amount green) blue
            | Blue amount -> aux t red green (max amount blue)
        end
    in let rec flatten_rounds = function
        | []  -> []
        | h :: t -> h.cubes @ flatten_rounds t
    in aux (flatten_rounds rounds) 0 0 0

let power_of_minimum (red, green, blue) =
    red * green * blue

let () =
    List.map convert_line_to_game lines
    |> List.map (fun game -> if is_valid_game game then game.id else 0)
    |> List.fold_left ( + ) 0
    |> Printf.sprintf "\nPart 1: %d"
    |> print_endline
    
let () =
    List.map convert_line_to_game lines
    |> List.map (fun game -> max_cubes_per_rounds game.rounds)
    |> List.map power_of_minimum
    |> List.fold_left ( + ) 0
    |> Printf.sprintf "\nPart 2: %d"
    |> print_endline
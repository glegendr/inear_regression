open Printf

let error str =
    print_endline ("\027[31mError:\027[0m " ^ str); exit 1

let rec print_data_list lst = match lst with
    | [] -> ()
    | (x::xs) -> (let (a, b) = x in printf "%i - %i\n" a b; print_data_list xs)

let read_file filename =
    let lines = ref [] in
    let chan = try
        open_in filename
    with Sys_error e -> error e
    in
    try
        while true; do
        lines := input_line chan :: !lines
        done; !lines
    with End_of_file ->
    close_in chan;
    List.rev !lines

let createData lst = match lst with
    | (x::x1::xs) -> ((try float_of_string x with Failure e -> error (x ^ " is not a valid number")), (try float_of_string x1 with Failure e -> error (x1 ^ " is not a valid number")))
    | _ -> error "String not well formated"

let parse file =
    let lst = read_file file in
    let rec loop m tail = match tail with
        | [] -> []
        | (x::xs) -> (
            let tmp_lst = String.split_on_char ',' (List.nth lst m) in
            if List.length tmp_lst <> 2 then error ("String not well formated L:" ^ string_of_int (m + 1))
            else if m = 0 then loop (m + 1) xs
            else [createData tmp_lst] @ loop (m + 1) xs)
    in loop 0 lst

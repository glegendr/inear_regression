let error str =
    print_endline ("\027[31mError:\027[0m " ^ str); exit 1

let read_file filename =
    let lines = ref [] in
    let chan = try
        open_in filename
    with Sys_error e -> error e
    in
    try
        while true; do
        lines := try input_line chan :: !lines with Sys_error e -> error (filename ^ " " ^ e)
        done; !lines
    with End_of_file ->
    close_in chan;
    List.rev !lines

let createData lst = match lst with
    | (x::x1::xs) -> ((try float_of_string x
        with Failure e -> error ("\"" ^ x ^ "\" is not a valid number")),
        (try float_of_string x1
        with Failure e -> error ("\"" ^ x1 ^ "\" is not a valid number")))
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

let args unit =
    let p = ref 0.000000001 in
    let r = ref 1000 in
    let rd = ref 1. in
    let d = ref false in
    let lst = ["-p", Arg.Set_float p, "[float]  precision"] @
        ["-r", Arg.Set_int r, "[int]    maximum loop"] @
        ["-d", Arg.Set d, "         display Tmp Teta"] @
        ["-t", Arg.Set_float rd, "[float]  training ratio"] in
    Arg.parse lst (fun _ -> ()) ("usage: " ^ (Sys.argv).(0) ^ " [-prdth]");
    (!p, !r, !rd, !d)

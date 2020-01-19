let in_file = "trained.csv"

let isHelp raw = match raw with
    | "-h" | "--help" -> print_endline ("Usage: " ^ (Sys.argv).(0) ^ " km"); exit 0
    | _ -> ()

let estMe data km =
    let (a, b) = List.nth data 0 in
    print_float (a +. b *. km);
    print_char '\n'

let () =
    let rawKm = try (Sys.argv).(1) with Invalid_argument e -> Parse.error "km not given" in
    isHelp rawKm;
    let km = try float_of_string rawKm
    with Failure e -> Parse.error (rawKm ^ " is not a valid number") in
    if km < 0. then Parse.error "km is negativ"
    else if Sys.file_exists in_file = true then estMe (Parse.parse in_file) km
    else estMe ([Parse.createData ["0"; "0"]]) km

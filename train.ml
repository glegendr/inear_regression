open Printf

let in_file = "data.csv"
let out_file = "trained.csv"

let square x = x *. x
let mult x y = x *. y

let rec print_list lst = match lst with
    | [] -> ()
    | (x::xs) -> printf "%f\n" x; print_list xs

let rec filter data pos = match (data, pos) with
    | ([], _) -> []
    | ((x::xs), 1) -> (let (a, _) = x in [a] @ filter xs pos)
    | ((x::xs), _) -> (let (_, b) = x in [b] @ filter xs pos)

let average data =
    let rec loop tail = match tail with
    | [] -> 0.
    | (x::xs) -> x +. loop xs
    in (loop data) /. float_of_int (List.length data)

let () =
    let data = Parse.parse in_file in
    let avX = average (filter data 1) in
    let avY = average (filter data 2) in
    let var = (average (List.map square (filter data 1))) -. square avX in
    let cov = average (List.map2 mult (filter data 1) (filter data 2)) -. (avX *. avY) in
    let oc = open_out out_file in
    fprintf oc "ui,non\n%f,%f" (avY -. (cov /. var) *. avX)  (cov /. var);
    close_out oc


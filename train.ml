open Printf

let in_file = "data.csv"
let out_file = "trained.csv"

let tet0 = ref 0.
let tet1 = ref 0.

let estimated_price x = !tet0 +. !tet1 *. x

let sup x y = if x > y then 1 else if x = y then 0 else -1
let inf x y = if x < y then 1 else if x = y then 0 else -1

let grad0 x y = (estimated_price x) -. y
let grad1 x y = (grad0 x y) *. x

let rec filter data pos = match (data, pos) with
    | ([], _) -> []
    | ((x::xs), 1) -> (let (a, _) = x in [a] @ filter xs pos)
    | ((x::xs), _) -> (let (_, b) = x in [b] @ filter xs pos)

let rec sum data = match data with
    | [] -> 0.
    | (x::xs) -> x +. sum xs

let average data = (sum data) /. float_of_int (List.length data)

let min_max data max =
    let rec loop tail = match tail with
        | [] -> []
        | (x::xs) -> [x /. max] @ loop xs
    in loop data

let print_me t0 t1 x d =
    if d = true then printf "Round %5i -> Teta0: %.10f -- Teta1: %.10f\n" x t0 t1
    else ()

let () =
    let (p, round, rd, d) = Parse.args() in
    let data = Parse.parse in_file in
    let maxk = List.hd (List.sort inf (filter data 1)) in
    let maxp = List.hd (List.sort inf (filter data 2)) in
    let km_list = min_max (filter data 1) maxk in
    let price_list = min_max (filter data 2) maxp in
    let rec loop x =
        let tmp0 = rd *. average (List.map2 grad0 km_list price_list) in
        let tmp1 = rd *. average (List.map2 grad1 km_list price_list) in
        tet0 := !tet0 -. tmp0;
        tet1 := !tet1 -. tmp1;
        print_me (!tet0 *. maxp) (!tet1 *. (maxp /. maxk)) x d;
        if tmp0 < p && tmp0 > (-.p) && tmp1 < p && tmp1 > (-.p) then ()
        else if x >= round then ()
        else loop (x + 1)
    in loop 0;
    let oc = try open_out out_file with Sys_error e -> Parse.error e in
    fprintf oc "Tet0,Tet1\n%f,%f" (!tet0 *. maxp) (!tet1 *. (maxp /. maxk));
    close_out oc

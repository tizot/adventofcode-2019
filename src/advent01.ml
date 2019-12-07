open Utils

let simple_fuel modl = modl / 3 - 2

let full_fuel modl =
  let rec aux m acc = let fuel = m / 3 - 2 in
  match fuel > 0 with
    | true -> aux fuel (fuel + acc)
    | false -> acc
  in aux modl 0

let total_fuel modules fuel_fn =
  List.map fuel_fn modules |> List.fold_left (+) 0

let () =
  let modules = List.map int_of_string (Utils.read_lines Sys.argv.(1)) in
  let fuel = total_fuel modules full_fuel in
  print_int fuel;
  print_newline()

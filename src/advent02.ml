open Utils

let update arr idx op =
  arr.(arr.(idx + 3)) <- op arr.(arr.(idx + 1)) arr.(arr.(idx + 2))

let traverse arr =
  let rec aux arr i =
    if (i >= Array.length arr)
    then failwith "Something went wrong, we are out of bounds..."
    else
      let op_code = arr.(i) in match op_code with
      | 99 -> arr
      | 1 -> update arr i ( + ); aux arr (i + 4)
      | 2 -> update arr i ( * ); aux arr (i + 4)
      | _ -> failwith "Invalid op code"
  in aux arr 0

let prepare arr =
  arr.(1) <- 12;
  arr.(2) <- 2;
  arr

let () =
  let raw_input = List.hd (Utils.read_lines (Sys.argv.(1))) in
  let splitted = String.split_on_char ',' raw_input in
  let intcode = Array.of_list (List.map int_of_string splitted)
  |> prepare
  |> traverse in
  let result = intcode.(0) in
  print_int result;
  print_newline ()

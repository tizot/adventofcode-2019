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

let prepare noun verb arr =
  arr.(1) <- noun;
  arr.(2) <- verb;
  arr

let output arr noun verb =
  Array.copy arr
  |> prepare noun verb
  |> traverse
  |> (fun a -> Array.get a 0)

let find arr =
  let rec iter tups = match tups with
    | [] -> None
    | (i, j) :: rest -> if (19690720 == (output arr i j)) then Some((i, j)) else iter rest
  in iter (cartesian (0 -- 99) (0 -- 99))

let () =
  let int_code = List.hd (read_lines (Sys.argv.(1)))
                 |> String.split_on_char ','
                 |> List.map int_of_string
                 |> Array.of_list in
  match (find int_code) with
  | None -> print_string "No result found"
  | Some((noun, verb)) -> print_int (100 * noun + verb);
    print_newline ()

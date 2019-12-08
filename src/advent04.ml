open Utils

let digits d =
  let rec dig acc d =
    if d < 10 then d :: acc
    else dig ((d mod 10) :: acc) (d / 10) in
  dig [] d

let initial l = List.rev l |> List.tl |> List.rev

let auto_zip l = List.combine (initial l) (List.tl l)

let is_six_digits num = (String.length (string_of_int num)) == 6

let does_not_decrease pair = (fst pair) <= (snd pair)

let is_equal_pair pair = (fst pair) == (snd pair)

let simple_duplicates pairs =
  let rec aux ps counts = match ps with
    | [] -> counts
    | p :: q -> if (is_equal_pair p)
      then counts.(fst p) <- counts.(fst p) + 1
      else ();
      aux q counts
  in let counts = aux pairs (Array.make 10 0) in
  Array.exists (fun x -> 1 == x) counts

let check num =
  if (not (is_six_digits num)) then false else
    let dgts = digits num in
    let pairs = auto_zip dgts in
    List.for_all does_not_decrease pairs && simple_duplicates pairs

let () =
  let result = List.length (List.filter check (265275 -- 781584)) in
  print_int result;
  print_newline ()

open Utils

let initial l = List.rev l |> List.tl |> List.rev

let auto_zip l = List.combine (initial l) (List.tl l)

let is_six_digits num = (String.length (string_of_int num)) == 6

let does_not_decrease pair = (fst pair) <= (snd pair)

let is_equal_pair pair = (fst pair) == (snd pair)

let check num =
  if (not (is_six_digits num)) then false else
  let digits = string_of_int num
               |> String.to_seq
               |> List.of_seq
               |> List.map int_of_char in
  let pairs = auto_zip digits in
  List.for_all does_not_decrease pairs && List.exists is_equal_pair pairs

let () =
  let result = List.length (List.filter check (265275 -- 781584)) in
  print_int result;
  print_newline ()

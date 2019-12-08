open Utils

type direction = Up | Left | Down | Right
type path = (direction * int) list
type coord = int * int
type interval = int * int (* first element is lesser than second *)
type segment = Vertical of (int * interval) | Horizontal of (interval * int)

let format_coord (c: coord) =
  let (x, y) = c in
  "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

let norm (c: coord) = let (x, y) = c in abs x + abs y

let move_by (orig: coord) steps dir = let (x0, y0) = orig in match dir with
 | Up -> (x0, y0 + steps)
 | Down -> (x0, y0 - steps)
 | Right -> (x0 + steps, y0)
 | Left -> (x0 - steps, y0)

let direction_of_char c = match c with
  | 'U' -> Up
  | 'L' -> Left
  | 'D' -> Down
  | 'R' -> Right
  | _ -> failwith "Invalid code for Direction"

let rec intersection seg1 seg2: coord option = match (seg1, seg2) with
  | Vertical (x1, _), Vertical (x2, _) when x1 != x2 -> None
  | Vertical (x1, (y11, y12)), Vertical (_, (y21, y22)) -> if (y11 > y21) then intersection seg2 seg1 else (
      if (y21 > y12) then None else Some(x1, y21)
    )
  | Horizontal (_, y1), Horizontal (_, y2) when y1 != y2 -> None
  | Horizontal ((x11, x12), y1), Horizontal ((x21, x22), _) -> if (x11 > x21) then intersection seg2 seg1 else (
      if (x21 > x12) then None else Some(x21, y1)
    )
  | Vertical (x1, (y11, y12)), Horizontal ((x21, x22), y2) ->
      if (x1 >= x21 && x1 <= x22 && y2 >= y11 && y2 <= y12) then Some (x1, y2) else None
  | _ -> intersection seg2 seg1

let decode_path l: path =
  let rec aux inp acc = match inp with
    | [] -> List.rev acc
    | m :: moves -> let (dir, steps) = (direction_of_char m.[0], String.sub m 1 (String.length m - 1)) in
      aux moves ((dir, int_of_string steps) :: acc)
  in aux l []

let segment_of_move move (origin: coord): segment = let (x0, y0) = origin in match move with
  | (Up, steps) -> Vertical (x0, (y0, y0 + steps))
  | (Down, steps) -> Vertical (x0, (y0 - steps, y0))
  | (Right, steps) -> Horizontal ((x0, x0 + steps), y0)
  | (Left, steps) -> Horizontal ((x0 - steps, x0), y0)

let build_segments (p: path) =
  let rec aux moves built current = match moves with
  | [] -> built
  | m :: ms -> let (dir, steps) = m in
    let seg = segment_of_move m current in
    aux ms (seg :: built) (move_by current steps dir)
  in aux p [] (0, 0) |> List.rev

let norm_compare a b = compare (norm a) (norm b)

let length s = match s with
  | Vertical (_, (y1, y2)) -> y2 - y1
  | Horizontal ((x1, x2), _) -> x2 - x1

let is_on s c = let (x0, y0) = c in match s with
  | Vertical (xs, (y1, y2)) -> xs == x0 && y1 <= y0 && y0 <= y2
  | Horizontal ((x1, x2), ys) -> ys == y0 && x1 <= x0 && x0 <= x2

let steps_to_target path target =
  let rec aux pth current dist = match pth with
    | [] -> failwith "Point is not on path"
    | move :: moves -> let seg = segment_of_move move current in
      if (is_on seg target)
      then (
        let (xt, yt) = target in
        let (xc, yc) = current in
        dist + abs (xt - xc) + abs (yt - yc)
      )
      else (
        let (dir, steps) = move in
        aux moves (move_by current steps dir) (dist + steps)
      )
  in aux path (0, 0) 0

let total_delay p1 p2 c = steps_to_target p1 c + steps_to_target p2 c

let delay_compare_factory p1 p2 a b =
  let delay_a = total_delay p1 p2 a in
  let delay_b = total_delay p1 p2 b in
  compare delay_a delay_b

let find_intersections p1 p2 compare =
  let seg1 = build_segments p1 in
  let seg2 = build_segments p2 in
  Utils.cartesian seg1 seg2
  |> List.filter_map (fun pair -> intersection (fst pair) (snd pair))
  |> List.sort compare

let decode_input inp = match inp with
  | [] -> failwith "Empty input"
  | _ :: [] -> failwith "Input contains only one line, two expected"
  | p1 :: p2 :: [] -> (
      decode_path (String.split_on_char ',' p1),
      decode_path (String.split_on_char ',' p2)
  )
  | _ -> failwith "Input contains more than 2 lines"

let () =
  let part = ref 1 in
  let in_file = ref "" in
  let arg_spec = [
    ("-i", Arg.Set_string in_file, "file containing the input data");
    ("-p", Arg.Set_int part, "part of the challenge to execute (1 or 2)")
  ] in
  Arg.parse arg_spec print_endline "Advent of code, day 3";
  let all_lines = Utils.read_lines !in_file in
  let (path1, path2) = decode_input all_lines in
  let compare_fn = if (!part == 2) then delay_compare_factory path1 path2 else norm_compare in
  let result_fn = if (!part == 2) then total_delay path1 path2 else norm in
  let intersections = find_intersections path1 path2 compare_fn in (
    match intersections with
    | origin :: inter :: _ -> print_int (result_fn inter)
    | _ -> print_string "No intersection found"
  );
  print_newline ()



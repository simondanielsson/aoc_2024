open Base
open Stdio

module Entity = struct
  type t =
    | Robot
    | Wall
    | Box
    | Empty
  [@@deriving sexp_of, equal]

  let of_char = function
    | '@' -> Robot
    | '#' -> Wall
    | 'O' -> Box
    | '.' -> Empty
    | x -> failwith (Stdlib.Format.sprintf "invalid symbol: %c" x)
  ;;
end

module Entity_p2 = struct
  type t =
    | Robot
    | Left_box
    | Right_box
    | Wall
    | Empty
  [@@deriving sexp_of, equal]

  let of_entity (e : Entity.t) =
    match e with
    | Robot -> Robot, Empty
    | Wall -> Wall, Wall
    | Box -> Left_box, Right_box
    | Empty -> Empty, Empty
  ;;

  let to_string = function
    | Robot -> "@"
    | Left_box -> "["
    | Right_box -> "]"
    | Wall -> "#"
    | Empty -> "."
  ;;
end

module Dir = struct
  type t =
    | Up
    | Down
    | Left
    | Right
  [@@deriving sexp_of, equal]

  let of_char = function
    | '^' -> Up
    | 'v' -> Down
    | '<' -> Left
    | '>' -> Right
    | x -> failwith (Stdlib.Format.sprintf "invalid symbol: %c" x)
  ;;

  let next_pos t (r, c) =
    match t with
    | Up -> r - 1, c
    | Down -> r + 1, c
    | Left -> r, c - 1
    | Right -> r, c + 1
  ;;
end

let read () =
  let lines = In_channel.(input_lines stdin) in
  let split_index, _ = List.findi_exn lines ~f:(fun _ line -> String.( = ) line "") in
  let map_str, dirs_str = List.partitioni_tf lines ~f:(fun i _ -> i < split_index) in
  let robot_pos = ref (-1, -1) in
  let create_map lines =
    let rows = List.length lines in
    let cols = String.length (List.hd_exn lines) in
    let map = Array.init rows ~f:(fun _ -> Array.init cols ~f:(fun _ -> Entity.Empty)) in
    List.iteri lines ~f:(fun r s ->
      String.iteri s ~f:(fun c value ->
        let entity = Entity.of_char value in
        map.(r).(c) <- entity;
        match entity with
        | Robot -> robot_pos := r, c
        | _ -> ()));
    map
  in
  let create_dirs lines =
    String.concat lines |> String.to_sequence |> Sequence.map ~f:Dir.of_char
  in
  (* First element of the dirs is the empty string *)
  let map = create_map map_str in
  let dirs = create_dirs (List.tl_exn dirs_str) in
  map, dirs, !robot_pos
;;

let move (map, dirs, robot_pos) =
  print_s [%sexp (map : Entity.t array array)];
  print_s [%sexp (dirs : Dir.t Sequence.t)];
  printf "Robot at (%d. %d)\n" (fst robot_pos) (snd robot_pos);
  let robot = ref robot_pos in
  let rec try_move dir (r, c) : bool =
    printf "From: (%d, %d) moving %s\n" r c (Dir.sexp_of_t dir |> Sexp.to_string);
    let r', c' = Dir.next_pos dir (r, c) in
    match map.(r').(c') with
    | Empty ->
      (* Ok, move was valid. Move from old to new. *)
      map.(r').(c') <- map.(r).(c);
      true
    | Wall ->
      (* Hit a wall, discard *)
      false
    | Box ->
      (* See if we can move this box *)
      if try_move dir (r', c')
      then (
        map.(r').(c') <- map.(r).(c);
        true)
      else false
    | Robot -> failwith "unreachable state: found robot"
  in
  Sequence.iter dirs ~f:(fun dir ->
    let valid_move = try_move dir !robot in
    printf "Valid move: %b\n" valid_move;
    if valid_move
    then (
      let r, c = !robot in
      map.(r).(c) <- Empty;
      robot := Dir.next_pos dir !robot);
    ());
  print_endline "After moving:";
  print_s [%sexp (map : Entity.t array array)];
  map
;;

let calculate_score map =
  let find_boxes map =
    Array.foldi map ~init:[] ~f:(fun r acc row ->
      Array.foldi row ~init:acc ~f:(fun c acc' value ->
        if Entity.equal value Box then (r, c) :: acc' else acc'))
  in
  let score_box (r, c) = (100 * r) + c in
  find_boxes map |> List.sum (module Int) ~f:score_box
;;

(* Part 2 *)
let upscale_map (map, dirs, _) =
  (* Twice as wide *)
  let robot_pos = ref (-1, -1) in
  let create_upscaled_map map =
    let orig_rows = Array.length map in
    let orig_cols = Array.length map.(0) in
    let upscaled_map =
      Array.init orig_rows ~f:(fun _ ->
        Array.init (orig_cols * 2) ~f:(fun _ -> Entity_p2.Empty))
    in
    Array.iteri map ~f:(fun r row ->
      Array.iteri row ~f:(fun c entity ->
        let e1, e2 = Entity_p2.of_entity entity in
        upscaled_map.(r).(2 * c) <- e1;
        upscaled_map.(r).((2 * c) + 1) <- e2;
        match e1 with
        | Robot -> robot_pos := r, 2 * c
        | _ -> ()));
    upscaled_map
  in
  let upscaled_map = create_upscaled_map map in
  upscaled_map, dirs, !robot_pos
;;

let print_map map =
  Array.iter map ~f:(fun row ->
    Array.iter row ~f:(fun entity -> printf "%s" (Entity_p2.to_string entity));
    print_endline "")
;;

let move_p2 (map, dirs, robot_pos) =
  print_map map;
  let robot = ref robot_pos in
  printf "Robot at (%d, %d)\n" (fst !robot) (snd !robot);
  let set_empty r c = map.(r).(c) <- Empty in
  let log r c r' c' =
    printf
      "Moving %s@(%d, %d) to (%d, %d), and setting former to .\n"
      (Entity_p2.to_string map.(r).(c))
      r
      c
      r'
      c'
  in
  let rec try_move dir (r, c) thunks =
    let r', c' = Dir.next_pos dir (r, c) in
    match map.(r').(c') with
    | Empty ->
      printf "Found . at (%d, %d)\n" r' c';
      let thunk () =
        log r c r' c';
        map.(r').(c') <- map.(r).(c);
        set_empty r c
      in
      (* printf "Found empty at (%d, %d)\n" r' c'; *)
      true, thunk :: thunks
    | Wall -> false, []
    | Right_box when Dir.equal dir Dir.Left || Dir.equal dir Dir.Right ->
      (*As before *)
      let move_valid, thunks = try_move dir (r', c') thunks in
      if move_valid
      then (
        let thunk () = map.(r').(c') <- map.(r).(c) in
        true, thunk :: thunks)
      else false, []
    | Left_box when Dir.equal dir Dir.Right || Dir.equal dir Dir.Left ->
      (* Same as before *)
      let move_valid, thunks = try_move dir (r', c') thunks in
      if move_valid
      then (
        let thunk () = map.(r').(c') <- map.(r).(c) in
        true, thunk :: thunks)
      else false, []
    | Left_box ->
      printf "Found [ at (%d, %d)\n" r' c';
      (* Going up on a left box is valid if the box is not blocked above and above and right *)
      let left_valid, left_thunks = try_move dir (r', c') thunks in
      let right_valid, right_thunks = try_move dir (r', c' + 1) thunks in
      if left_valid && right_valid
      then (
        let t1 () =
          log r c r' c';
          map.(r').(c') <- map.(r).(c);
          set_empty r c
        in
        let t2 () =
          printf "pushing at [ with %d thunks in right\n" (List.length right_thunks);
          log r (c + 1) r' (c' + 1);
          if Entity_p2.equal map.(r).(c + 1) Empty
          then (
            map.(r').(c' + 1) <- map.(r).(c + 1);
            set_empty r (c + 1))
        in
        true, (t1 :: t2 :: right_thunks) @ left_thunks)
      else false, []
    | Right_box ->
      printf "Found ] at (%d, %d)\n" r' c';
      (* Going up on a left box is valid if the box is not blocked above and above and right *)
      let left_valid, left_thunks = try_move dir (r', c') thunks in
      let right_valid, right_thunks = try_move dir (r', c' - 1) thunks in
      if left_valid && right_valid
      then (
        let t1 () =
          log r c r' c';
          map.(r').(c') <- map.(r).(c);
          set_empty r c
        in
        let t2 () =
          log r (c - 1) r' (c' - 1);
          if Entity_p2.equal map.(r).(c + 1) Empty
          then (
            map.(r').(c' - 1) <- map.(r).(c - 1);
            set_empty r (c - 1))
        in
        true, (t1 :: t2 :: left_thunks) @ right_thunks)
      else false, []
    | Robot -> failwith "unreachable state: found robot"
  in
  (* let set_empty (rr, rc) (dir : Dir.t) = *)
  (*   (* printf "Setting empty from (%d, %d)\n" rr rc; *) *)
  (*   match dir with *)
  (*   | Up when Entity_p2.equal map.(rr - 1).(rc) Left_box -> map.(rr).(rc + 1) <- Empty *)
  (*   | Up when Entity_p2.equal map.(rr - 1).(rc) Right_box -> map.(rr).(rc - 1) <- Empty *)
  (*   | Down when Entity_p2.equal map.(rr + 1).(rc) Left_box -> map.(rr).(rc + 1) <- Empty *)
  (*   | Down when Entity_p2.equal map.(rr + 1).(rc) Right_box -> map.(rr).(rc - 1) <- Empty *)
  (*   | _ -> () (*print_endline "no match"*) *)
  (* in *)
  (* Same as in part 1 *)
  Sequence.iter dirs ~f:(fun dir ->
    print_s [%sexp (dir : Dir.t)];
    let valid_move, thunks = try_move dir !robot [] in
    printf "Thunks: %d\n" (List.length thunks);
    List.iter (List.rev thunks) ~f:(fun t ->
      t ();
      ());
    (* printf "Tried moving: %s\n" (Dir.sexp_of_t dir |> Sexp.to_string); *)
    (* printf "Valid move: %b\n" valid_move; *)
    if valid_move
    then (
      let r, c = !robot in
      map.(r).(c) <- Empty;
      robot := Dir.next_pos dir !robot
      (* if List.length thunks > 1 *)
      (* then *)
      (*   set_empty !robot dir *)
      (* printf "Robot at (%d, %d)\n" (fst !robot) (snd !robot) *));
    (* print_map map; *)
    ());
  print_endline "After moving:";
  print_map map;
  map
;;

(* Same as for p1, but find left side of box *)
let calculate_score_p2 map =
  let find_boxes map =
    Array.foldi map ~init:[] ~f:(fun r acc row ->
      Array.foldi row ~init:acc ~f:(fun c acc' value ->
        if Entity_p2.equal value Left_box then (r, c) :: acc' else acc'))
  in
  let score_box (r, c) = (100 * r) + c in
  find_boxes map |> List.sum (module Int) ~f:score_box
;;

let () =
  (* Part 1 *)
  ignore move;
  ignore calculate_score;
  (* let result = read () |> move |> calculate_score in *)
  (* Part 2 *)
  let result = read () |> upscale_map |> move_p2 |> calculate_score_p2 in
  printf "Result: %d\n" result
;;

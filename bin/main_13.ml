open Base
open Stdio

module Coords = struct
  type t =
    { x : int
    ; y : int
    }
  [@@deriving sexp_of]
end

module Button = struct
  (* Offset in coords *)
  type t =
    { offsets : Coords.t
    ; cost : int
    }
  [@@deriving sexp_of]
end

module Machine = struct
  type t =
    { button_A : Button.t
    ; button_B : Button.t
    ; prize : Coords.t
    }
  [@@deriving sexp_of]
end

module Solution = struct
  type t =
    { count_A : int
    ; count_B : int
    }
  [@@deriving sexp_of, hash, equal, compare]

  let cost t machine =
    List.sum
      (module Int)
      Machine.[ t.count_A * machine.button_A.cost; t.count_B * machine.button_B.cost ]
      ~f:Fn.id
  ;;
end

module Constants = struct
  let re_xy_plus = Re.Perl.compile_pat "X\\+(\\d+), Y\\+(\\d+)"
  let re_xy_eq = Re.Perl.compile_pat "X=(\\d+), Y=(\\d+)"
  let a_cost = 3
  let b_cost = 1
end

let read () : Machine.t list =
  let lines = In_channel.input_lines In_channel.stdin in
  let parse_coords re line =
    match Re.exec_opt re line with
    | Some group ->
      Coords.
        { x = Re.Group.get group 1 |> Int.of_string
        ; y = Re.Group.get group 2 |> Int.of_string
        }
    | None -> failwith "failed to parse numbers from X+, Y+"
  in
  let rec loop acc lines =
    let parse_machine a b prize =
      let a_coords = parse_coords Constants.re_xy_plus a in
      let b_coords = parse_coords Constants.re_xy_plus b in
      let prize_coords = parse_coords Constants.re_xy_eq prize in
      Machine.
        { button_A = Button.{ offsets = a_coords; cost = Constants.a_cost }
        ; button_B = { offsets = b_coords; cost = Constants.b_cost }
        ; prize = prize_coords
        }
    in
    match lines with
    | a :: b :: prize :: _ :: tail ->
      let machine = parse_machine a b prize in
      loop (machine :: acc) tail
    | [ a; b; prize ] -> parse_machine a b prize :: acc
    | _ -> failwith "invalid input format"
  in
  (* TODO: remove *)
  List.rev @@ loop [] lines
;;

let calculate_cost_dir a_offset_dir b_offset_dir prize =
  let matrix_x =
    Array.init 101 ~f:(fun a ->
      Array.init 101 ~f:(fun b ->
        match b, a with
        | 0, 0 -> 0
        | 0, _a -> _a * a_offset_dir
        | _b, 0 -> _b * b_offset_dir
        | _ -> -1))
  in
  let solutions = ref [] in
  let rec loop a b : unit =
    if b > Array.length matrix_x - 1
    then loop (a + 1) (b - 1)
    else if a > Array.length matrix_x.(0) - 1
    then print_s [%sexp (matrix_x : int array array)]
    else (
      let value = (a * a_offset_dir) + (b * b_offset_dir) in
      matrix_x.(a).(b) <- value;
      if value = prize
      then solutions := Solution.{ count_A = a; count_B = b } :: !solutions;
      if value < prize && b < Array.length matrix_x - 1
      then loop a (b + 1)
      else loop (a + 1) 0)
  in
  loop 0 1;
  !solutions
;;

let calculate_cost (machine : Machine.t) : int =
  let x_sols =
    calculate_cost_dir
      machine.button_A.offsets.x
      machine.button_B.offsets.x
      machine.prize.x
  in
  let y_sols =
    calculate_cost_dir
      machine.button_A.offsets.y
      machine.button_B.offsets.y
      machine.prize.y
  in
  let x_set = Hash_set.of_list (module Solution) x_sols in
  let y_set = Hash_set.of_list (module Solution) y_sols in
  let common_sols = Hash_set.inter x_set y_set in
  print_s [%sexp (x_sols : Solution.t list)];
  print_s [%sexp (y_sols : Solution.t list)];
  print_s [%sexp (common_sols : Solution.t Hash_set.t)];
  if Hash_set.is_empty common_sols
  then 0
  else (
    match
      Hash_set.min_elt common_sols ~compare:(fun s1 s2 ->
        Solution.cost s1 machine - Solution.cost s2 machine)
    with
    | Some sol -> Solution.cost sol machine
    | None -> 0)
;;

(* Find smallest cost to win as many prices as possible?
   Cost A: 3; Cost B: 1 

  (100, 100) grid
  Try backtracking by exploring B first

  Problem: minimize 3*count_A + 1*count_B
  Where count_A*A_x + count_B*B_x = prize_X
        count_A <= 100
        count_B <= 100
  
  Typical linear program

  Either pick A or not

  pick A if
    (count_A - 1) * A_x + count_B*B_x = prize_X - A_x
  
  X and Y could be solved in parallel.

   No button to be pressed more than 100 times.
*)
let () =
  let result = read () |> List.sum (module Int) ~f:calculate_cost in
  printf "Result: %d\n" result
;;

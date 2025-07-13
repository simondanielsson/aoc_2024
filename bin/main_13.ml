open Base
open Stdio

module Coords = struct
  type t =
    { x : int
    ; y : int
    }
  [@@deriving sexp_of]

  let add ~amount t = if amount = 0 then t else { x = t.x + amount; y = t.y + amount }
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

  (* 0 for part 1 *)
  let part2_offset = 10000000000000
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
        ; prize = Coords.add ~amount:Constants.part2_offset prize_coords
        }
    in
    match lines with
    | a :: b :: prize :: _ :: tail ->
      let machine = parse_machine a b prize in
      loop (machine :: acc) tail
    | [ a; b; prize ] -> parse_machine a b prize :: acc
    | _ -> failwith "invalid input format"
  in
  List.rev @@ loop [] lines
;;

(* Solve a 2x2 system of linear equations using Cramer's rule *)
let solve_2x2 a11 a12 a21 a22 b1 b2 =
  let det = (a11 *. a22) -. (a12 *. a21) in
  if Float.( = ) det 0.0
  then None
  else (
    let x = ((b1 *. a22) -. (b2 *. a12)) /. det in
    let y = ((a11 *. b2) -. (a21 *. b1)) /. det in
    if Float.(is_integer x && is_integer y)
    then (
      let a = Int.of_float x in
      let b = Int.of_float y in
      if Constants.part2_offset = 0
      then if a > 100 || b > 100 then None else Some (a, b)
      else Some (a, b))
    else None)
;;

let calculate_cost_opt (machine : Machine.t) =
  let open Option.Let_syntax in
  let%map a, b =
    solve_2x2
      (Float.of_int machine.button_A.offsets.x)
      (Float.of_int machine.button_B.offsets.x)
      (Float.of_int machine.button_A.offsets.y)
      (Float.of_int machine.button_B.offsets.y)
      (Float.of_int machine.prize.x)
      (Float.of_int machine.prize.y)
  in
  if a >= 0 && b >= 0
  then Solution.cost Solution.{ count_A = a; count_B = b } machine
  else 0
;;

let calculate_cost machine = calculate_cost_opt machine |> Option.value ~default:0

(* Find smallest cost to win as many prices as possible?
   Cost A: 3; Cost B: 1 

  (100, 100) grid
  Try backtracking by exploring B first

  Problem: minimize 3*count_A + 1*count_B
  Where count_A*A_x + count_B*B_x = prize_X
        count_A <= 100
        count_B <= 100
  
  Typical linear program

   C = (count_A ; count_B)

   A_x  B_x  |  count_A       prize_x
             |            =   
   A_y  B_y  |  count_B       prize_y

   <=>

   M c = P

   =>

   c = M^(-1) P

   or since it's a 2x2 we can use Cramer's rule:
*)
let () =
  let result = read () |> List.sum (module Int) ~f:calculate_cost in
  printf "Result: %d\n" result
;;

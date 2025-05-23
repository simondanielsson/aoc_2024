open Base
open Stdio

let read () = In_channel.input_all In_channel.stdin

let full_pattern =
  Re.Perl.compile_pat "mul\\((\\d{1,3}),(\\d{1,3})\\)|(do\\(\\))|(don\'t\\(\\))"
;;

type match_type =
  | Do
  | Dont
  | Mul of int * int
[@@deriving sexp_of]

let classify_match program =
  match Re.exec_opt full_pattern program with
  | Some group ->
    (match Re.Group.get group 0 with
     | "do()" -> Some Do
     | "don't()" -> Some Dont
     | m when String.is_substring ~substring:"mul" m ->
       let x = Re.Group.get group 1 |> Int.of_string in
       let y = Re.Group.get group 2 |> Int.of_string in
       Some (Mul (x, y))
     | _ -> None)
  | None -> None
;;

let parse_pattern program =
  (* Continuously find matches and update position to continue on *)
  let rec aux ~enabled acc pos =
    match Re.exec_opt ~pos full_pattern program with
    (* Find next match *)
    | None -> List.rev acc
    | Some group ->
      let type_of_match = Re.Group.get group 0 |> classify_match in
      let next_pos = Re.Group.stop group 0 in
      (match type_of_match with
       | Some Do -> aux ~enabled:true acc next_pos
       | Some Dont -> aux ~enabled:false acc next_pos
       | Some (Mul (_, _)) when not enabled -> aux ~enabled acc next_pos (* Skip *)
       | Some (Mul (x, y)) when enabled -> aux ~enabled ((x * y) :: acc) next_pos
       | Some _ | None -> acc)
  in
  aux ~enabled:true [] 0
;;

let sum lst = List.fold lst ~init:0 ~f:(fun acc m -> acc + m)

let () =
  let result = read () |> parse_pattern |> sum in
  printf "Result: %d\n" result
;;

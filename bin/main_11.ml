open Base
open Stdio

let num_blinks = 25

module Stone = struct
  type t =
    | Zero
    | Even_digits of int
    | Other of int
  [@@deriving sexp_of]

  let of_string s =
    if String.for_all s ~f:(fun c -> Char.( = ) c '0')
    then Zero
    else if String.length (Int.to_string @@ Int.of_string s) % 2 = 0
    then Even_digits (Int.of_string s)
    else Other (Int.of_string s)
  ;;

  let to_string = function
    | Zero -> "0"
    | Even_digits x | Other x -> Int.to_string x
  ;;

  let transform = function
    | Zero -> [ Other 1 ]
    | Even_digits x ->
      let x_str = Int.to_string x in
      let len = String.length x_str in
      let chars = String.to_array x_str in
      let fst = ref [] in
      let snd = ref [] in
      for i = 0 to (len / 2) - 1 do
        fst := String.of_char chars.(i) :: !fst;
        snd := String.of_char chars.((len / 2) + i) :: !snd
      done;
      List.rev
        [ of_string (String.rev (String.concat !fst))
        ; of_string (String.rev (String.concat !snd))
        ]
    | Other x ->
      let value = 2024 * x in
      if String.length (Int.to_string value) % 2 = 0
      then [ Even_digits value ]
      else [ Other value ]
  ;;
end

let read () =
  let s = In_channel.input_line_exn In_channel.stdin in
  print_endline s;
  let stones = String.split_on_chars ~on:[ ' ' ] s in
  List.map stones ~f:Stone.of_string
;;

(*
   Each stone:
    - 0 -> 1
    - even number of digits -> (l, r)   E.g.   1222 -> (12, 22); 1200 -> (12, 0)
    - x -> 2024 * x

  Q: How many stones after 25 blinks?
*)

let blink stones =
  print_s [%sexp (stones : Stone.t list)];
  let rec loop ~remaining stones =
    match remaining with
    | 0 -> stones
    | _ ->
      printf "Remaining: %d\n" remaining;
      let new_stones =
        List.rev
        @@ List.fold stones ~init:[] ~f:(fun acc stone -> Stone.transform stone @ acc)
      in
      let new_stones_hum = List.map new_stones ~f:Stone.to_string in
      print_s [%sexp (new_stones_hum : string list)];
      print_s [%sexp (new_stones : Stone.t list)];
      loop ~remaining:(remaining - 1) new_stones
  in
  ignore num_blinks;
  let res = loop ~remaining:6 stones in
  let res_hus = List.map res ~f:Stone.to_string in
  print_s [%sexp (res_hus : string list)];
  res
;;

let () =
  let num_stones = read () |> blink |> List.length in
  printf "Stones: %d\n" num_stones
;;

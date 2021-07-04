open Core

exception UnwrapFiled

let force_unwrap_unequal_lengths =
  function
  | List.Or_unequal_lengths.Ok(v) -> v
  | List.Or_unequal_lengths.Unequal_lengths -> raise UnwrapFiled

type 'a check_digit_res = 
  | Ok of 'a
  | InvalidRemainder

let calculate_check_digit from =
  let weight = [3; 2; 7; 6; 5; 4; 3; 2] in
  let sum = List.init (String.length from) ~f: (String.get from) |>
    List.zip weight |>
    force_unwrap_unequal_lengths |>
    List.map ~f: (fun (w, c) -> Int.of_string (String.of_char c) * w) |>
    List.fold ~init: 0 ~f: (fun a b -> a + b) in
  
  let remainder = sum % 11 in

  match remainder with
  | 0 -> Ok(0)
  | 1 -> InvalidRemainder
  | _ -> Ok(11 - remainder)

let to_two_digit_string value =
  let str_val = Int.to_string value in

  match String.length str_val with 
  | 1 -> "0" ^ str_val
  | _ -> str_val

let rec get_two_random_and_check_digit date_part two_random =
  let ssn_until_check_digit = date_part ^ Int.to_string two_random in

  match calculate_check_digit ssn_until_check_digit with
  | Ok(check_digit) -> (two_random, check_digit)
  | InvalidRemainder when two_random = 99 ->
    get_two_random_and_check_digit date_part 20
  | InvalidRemainder -> get_two_random_and_check_digit date_part (two_random + 1) ;;

let generate_ssn from_year to_year = 
  (* Date part *)
  let day = Random.int_incl 1 28 |> to_two_digit_string in
  let month = Random.int_incl 1 12 |> to_two_digit_string in
  let full_year = Random.int_incl from_year to_year |> Int.to_string in
  let year_last_two = String.sub full_year ~pos: 2 ~len: 2 in

  (* Random and check_digit *)
  let (two_random, check_digit) = 
    get_two_random_and_check_digit 
      (day ^ month ^ year_last_two)
      (Random.int_incl 20 99) in

  (* Last digit(century) *)
  let century = String.sub full_year ~pos: 1 ~len: 1 in

  day ^
    month ^
    year_last_two ^
    Int.to_string two_random ^
    Int.to_string check_digit ^
    century

let rec generate_multiple count from_year to_year =
  let rec generate_unique_ssn ssn_list = (
    let ssn = generate_ssn from_year to_year in
    if not (List.mem ssn_list ssn ~equal:(String.equal)) then
      ssn
    else generate_unique_ssn ssn_list
  ) in
  
  match count with
  | 0 -> []
  | _ -> 
    let current_list = generate_multiple (count - 1) from_year to_year in
    let ssn = generate_unique_ssn current_list in
    current_list @ [ssn]
  
let is_number str =
  Str.string_match (Str.regexp "[0-9]+") str 0

let () =
  let args = Sys.get_argv() in
  let count = if Array.length args > 1 && is_number args.(1) then
    Int.of_string args.(1)
  else 
    1 in
  let ssn_list = generate_multiple count 2012 2020 in
  List.iter ssn_list ~f: (fun ssn -> print_endline ssn)

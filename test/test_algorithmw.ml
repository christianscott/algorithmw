open Algorithmw

type test_result = Failure of string | Success

let red s = Printf.sprintf "\x1B[31m%s\x1B[0m" s

let check (expr, t) =
  let inferred = infer expr in
  if not (inferred = t) then
    let msg =
      Printf.sprintf "expected %s, got %s" (string_of_type t)
        (string_of_type inferred)
    in
    Failure msg
  else Success

let int_lit = ELiteral (LInt 0)

let bool_lit = ELiteral (LBool true)

let cases =
  [
    (int_lit, TInt);
    (bool_lit, TBool);
    ( (* \x -> 0 *)
      ELambda ("x", int_lit),
      (* 'a -> int *)
      TFunction (TVariable "a", TInt) );
    ( (* \x -> x *)
      ELambda ("x", EVariable "x"),
      (* 'a -> 'a *)
      TFunction (TVariable "a", TVariable "a") );
    ((* let x = 0 in x *)
     ELet ("x", int_lit, EVariable "x"), (* int *)
                                         TInt);
    ( (* \x -> \y -> 0 *)
      ELambda ("x", ELambda ("x", int_lit)),
      (* 'a -> 'b -> int *)
      TFunction (TVariable "a", TFunction (TVariable "b", TInt)) );
    ( (* \x -> x 0 *)
      ELambda ("x", EApplication (EVariable "x", int_lit)),
      (* int -> 'a -> 'a *)
      TFunction (TFunction (TInt, TVariable "a"), TVariable "a") );
    ( (* let f = \x -> x in f 0 *)
      ELet
        ( "f",
          ELambda ("x", EVariable "x"),
          EApplication (EVariable "f", int_lit) ),
      (* int *)
      TInt );
  ]

let report_failures results ~n_tests ~n_failed =
  let () =
    let mk_dot r = if r == Success then "·" else red "x" in
    let dots = String.concat "" (List.map mk_dot results) in
    Printf.printf "%s\n" dots
  in
  Printf.printf "%d out of %d tests passing\n\n" (n_tests - n_failed) n_tests;
  for i = 0 to (n_tests - 1) do
    match List.nth results i with
    | Failure msg -> Printf.printf "test %d:\n  %s\n\n" (i + 1) (red msg)
    | _ -> ()
  done

let run_suite suite =
  let n_tests = List.length suite in
  let results = List.map check suite in
  let failed = List.filter (fun x -> x != Success) results in
  let n_failed = List.length failed in
  if n_failed == 0 then Printf.printf "all tests passed"
  else report_failures results ~n_tests ~n_failed;
  failwith "test suite failed"

let () = run_suite cases

open Algorithmw

type test_result = Failure | Success

let () =
  let int_literal = ELiteral (LInt 0) in
  let bool_literal = ELiteral (LBool true) in
  let cases =
    [
      (int_literal, TInt);
      (bool_literal, TBool);
      (
        (* \x -> 0 *)
        ELambda ("x", int_literal),
        (* 'a -> int *)
        TFunction (TVariable "a", TInt)
      );
      (
        (* \x -> x 0 *)
        ELambda ("x", (EApplication (EVariable "x", int_literal))),
        (* int -> 'a -> 'a *)
        TFunction ((TFunction (TInt, TVariable "a")), (TVariable "a"))
      );
    ]
  in
  let check (expr, t) =
    let inferred = infer expr in
    if not (inferred = t) then
      let _ =
        Printf.printf "expected %s, got %s\n" (string_of_type t)
          (string_of_type inferred)
      in
      Failure
    else Success
  in
  let results = List.map check cases in
  let succeeded = List.filter (fun x -> x == Success) results in
  if List.length succeeded == List.length results then
    Printf.printf "all tests passed"
  else
    let _ =
      Printf.printf "%d tests out of %d tests failed\n" (List.length succeeded)
        (List.length results)
    in
    failwith "test suite failed"

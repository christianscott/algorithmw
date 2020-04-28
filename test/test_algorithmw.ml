open Algorithmw

type test_result =
  | Failure
  | Success

let () =
  let int_literal = ELiteral (LInt 0) in
  let bool_literal = ELiteral (LBool true) in
  let cases = [
      (int_literal, TInt);
      (bool_literal, TBool);
      (ELambda ("x", int_literal), TFunction (TVariable ("a"), TInt));
  ] in
  let check (expr, t) =
    let (_, inferred) = infer Subst.null expr in
    if not (inferred = t) then
      let _ = Printf.printf "expected %s, got %s\n" (string_of_type t) (string_of_type inferred) in
      Failure
    else
      Success
  in
  let results = List.map check cases in
  let succeeded = List.filter (fun result -> result == Success) results in
  if (List.length succeeded) == (List.length results) then
    Printf.printf "all tests passed"
  else
    let _ = Printf.printf "%d tests out of %d tests failed\n" (List.length succeeded) (List.length results) in
    failwith "test suite failed"

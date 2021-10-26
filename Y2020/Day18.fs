namespace AdventOfCode

module Day18 =
  open FParsec

  type Expression =
    | Add of Expression * Expression
    | Multiply of Expression * Expression
    | Value of int

  let rec sprintExpression expression =
    match expression with
    | Add      (Value i1, Value i2) -> sprintf $"{i1} + {i2}"
    | Add      (Value i , e       ) -> sprintf $"{i} + ({sprintExpression e})"
    | Add      (e       , Value i ) -> sprintf $"({sprintExpression e} + {i}"
    | Add      (e1      , e2      ) -> sprintf $"({sprintExpression e1}) + ({sprintExpression e2})"
    | Multiply (Value i1, Value i2) -> sprintf $"{i1} * {i2}"
    | Multiply (Value i , e       ) -> sprintf $"{i} * ({sprintExpression e})"
    | Multiply (e       , Value i ) -> sprintf $"({sprintExpression e} * {i}"
    | Multiply (e1      , e2      ) -> sprintf $"({sprintExpression e1}) * ({sprintExpression e2})"
    | Value     i                   -> sprintf $"{i}"


  let rec evaluate expression =
    printfn "\nExpression: %s" (sprintExpression expression)
    match expression with
    | Add      (Value i1, Value i2) -> i1 + i2
    | Add      (Value i , e       )
    | Add      (e       , Value i ) -> (evaluate e ) + i
    | Add      (e1      , e2      ) -> (evaluate e1) + (evaluate e2)
    | Multiply (Value i1, Value i2) -> i1 * i2
    | Multiply (Value i , e       )
    | Multiply (e       , Value i ) -> (evaluate e ) * i
    | Multiply (e1      , e2      ) -> (evaluate e1) * (evaluate e2)
    | Value     i                   -> i

  let run () =

    Add (Multiply (Value 4, Value 5), Multiply (Value 2, Value 3))
    |> evaluate
    |> pso "evaluate: "
    |> ignore

    /// 2 * 3 + (4 * 5)
    Add ( Multiply ( Value 2, Value 3), Multiply (Value 4, Value 5))
    |> evaluate
    |> pso "evaluate: "
    |> ignore

    let push value l = value::l
    let pop value l = List.head l

    let test p str =
      match run p str with
      | Success(result, _, _)   -> printfn "Success: %A" result
      | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

    test pfloat "1.25"


///   6   + (4 * 5)
///   6   +  20
///   26






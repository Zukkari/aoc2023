module DayOne

open System.IO

// Part one
let lines = File.ReadLines "AdventOfCode/Day1/input_full.txt" |> seq

let numbers = seq { 0..1..9 } |> Seq.map string

let isNumber s = Seq.contains s numbers

let calibrationValue (x: seq<string>) =
    let first = Seq.find isNumber x in
    let last = Seq.findBack isNumber x in
    (string first) + (string last)

let solution =
    lines
    |> Seq.map (fun x -> Seq.toList x |> Seq.map string |> calibrationValue)
    |> Seq.map int
    |> Seq.sum

// Part two

type Number = { Numeric: int; Letter: string }

let letterNumbers =
    [ { Numeric = 0; Letter = "zero" }
      { Numeric = 1; Letter = "one" }
      { Numeric = 2; Letter = "two" }
      { Numeric = 3; Letter = "three" }
      { Numeric = 4; Letter = "four" }
      { Numeric = 5; Letter = "five" }
      { Numeric = 6; Letter = "six" }
      { Numeric = 7; Letter = "seven" }
      { Numeric = 8; Letter = "eight" }
      { Numeric = 9; Letter = "nine" } ]

let rec firstNumber (s: string) =
    match List.tryFind (fun x -> s.StartsWith x.Letter) letterNumbers with
    | Some(x) -> x.Numeric
    | _ ->
        match Seq.tryFind (fun x -> string x |> s.StartsWith) numbers with
        | Some(x) -> int x
        | _ -> Seq.tail s |> Seq.map string |> String.concat "" |> firstNumber

let rec lastNumber (s: string) =
    match List.tryFind (fun x -> s.EndsWith x.Letter) letterNumbers with
    | Some(x) -> x.Numeric
    | _ ->
        match Seq.tryFind (fun x -> string x |> s.EndsWith) numbers with
        | Some(x) -> int x
        | _ ->
            Seq.rev s
            |> Seq.tail
            |> Seq.rev
            |> Seq.map string
            |> String.concat ""
            |> lastNumber

let secondCalibrationValue s =
    let first = firstNumber s in
    let last = lastNumber s in
    (string first) + (string last)

let secondSolution =
    lines |> Seq.map secondCalibrationValue |> Seq.map int |> Seq.sum

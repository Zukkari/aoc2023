namespace Day2

open System.Drawing

module Game =
    type CubeColor =
        | Green
        | Red
        | Blue

    type CubeCombination = { Count: int; Color: CubeColor }

    type GamePart = seq<CubeCombination>

    type Game = { Id: int; Parts: seq<GamePart> }


module Parse =
    open System.IO
    open Game

    let lines f = File.ReadLines f |> seq

    let parseId (s: string) =
        s.Split [| ' ' |] |> Array.toList |> List.rev |> List.head |> int

    let parseColor =
        function
        | "blue" -> Blue
        | "red" -> Red
        | "green" -> Green
        | _ -> failwith "unsupported color"

    let toCubeCombination (s: string) =
        match s.Split [| ' ' |] |> Array.toList with
        | x :: [ xs ] ->
            let color = parseColor xs in
            let count = int x in
            { Count = count; Color = color }
        | _ -> "Invalid combination: " + s |> failwith

    let parsePart (part: string) =
        part.Split [| ',' |]
        |> List.ofArray
        |> List.map (fun x -> x.Trim())
        |> List.map toCubeCombination
        |> Seq.ofList

    let parseGameParts (s: string) =
        s.Split [| ';' |] |> List.ofArray |> List.map parsePart |> Seq.ofList

    let parseGame (s: string) =
        match s.Split [| ':' |] |> Array.toList with
        | x :: [ xs ] ->
            let id = parseId x in
            let parts = parseGameParts xs in
            { Id = id; Parts = parts }
        | _ -> "Invalid game: " + s |> failwith

    let games f = lines f |> Seq.map parseGame

module PartOne =
    open Game

    let validCount =
        function
        | { Count = count; Color = Blue } -> count <= 14
        | { Count = count; Color = Red } -> count <= 12
        | { Count = count; Color = Green } -> count <= 13

    let isPossible { Parts = parts } =
        Seq.forall (Seq.forall validCount) parts

    let solve f =
        Parse.games f |> Seq.filter isPossible |> Seq.fold (fun acc x -> acc + x.Id) 0

module PartTwo =
    open Game

    type MinimalViableProduct = { Blue: int; Red: int; Green: int }

    let mvpMax color parts =
        Seq.filter (fun x -> x.Color = color) parts
        |> Seq.fold (fun acc x -> max acc x.Count) 0

    let mvp { Parts = parts } =
        let flat = Seq.collect id parts in

        let cubeCount =
            { Blue = mvpMax Blue flat
              Red = mvpMax Red flat
              Green = mvpMax Green flat } in

        cubeCount.Blue * cubeCount.Red * cubeCount.Green

    let solve f =
        Parse.games f |> Seq.map mvp |> Seq.fold (+) 0

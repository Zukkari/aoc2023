namespace Day2

open Xunit

module ParseTests =
    open Parse
    open Game

    [<Fact>]
    let ``parseId returns number`` () = Assert.Equal(2, parseId "2")

    [<Fact>]
    let ``parseColor returns color`` () =
        Assert.Equal(CubeColor.Blue, parseColor "blue")

    [<Fact>]
    let ``toCubeCombination parses line`` () =
        let line = "2 green" in
        let expected = { Count = 2; Color = Green } in
        Assert.Equal(expected, toCubeCombination line)

    [<Fact>]
    let ``parsePart parses multiple combinations`` () =
        let line = "2 green, 3 red, 5 blue" in

        let expected =
            [ { Count = 2; Color = Green }
              { Count = 3; Color = Red }
              { Count = 5; Color = Blue } ] in

        Assert.Equal(expected, parsePart line)

    [<Fact>]
    let ``parseGameParts parses multiple games`` () =
        let line = "2 green, 3 red; 5 blue" in

        let expected =
            [ seq {
                  { Count = 2; Color = Green }
                  { Count = 3; Color = Red }
              }
              seq { { Count = 5; Color = Blue } } ] in

        Assert.Equal(expected, parseGameParts line)


    [<Fact>]
    let ``parseGame parses game successfully`` () =
        let line = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" in

        let expected =
            { Id = 1
              Parts =
                seq {
                    seq {
                        { Count = 3; Color = Blue }
                        { Count = 4; Color = Red }
                    }

                    seq {
                        { Count = 1; Color = Red }
                        { Count = 2; Color = Green }
                        { Count = 6; Color = Blue }
                    }

                    seq { { Count = 2; Color = Green } }
                } } in

        Assert.Equivalent(expected, parseGame line)

module PartOneTests =
    open PartOne

    [<Fact>]
    let ``solve solves small input correctly`` () =
        Assert.Equal(8, solve "input_small.txt")

    [<Fact>]
    let ``solve solves full input correctly`` () =
        Assert.Equal(1867, solve "input_full.txt")

module PartTwoTests =
    open PartTwo

    [<Fact>]
    let ``solve solves small input correctly`` () =
        Assert.Equal(2286, solve "input_small.txt")

    [<Fact>]
    let ``solve solves full input correctly`` () =
        Assert.Equal(84538, solve "input_full.txt")

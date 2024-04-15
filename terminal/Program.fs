module TicTacTerminal

open TicTac

type System.String with
    member s1.icompare(s2: string) =
        System.String.Equals(
            s1,
            s2,
            System.StringComparison.CurrentCultureIgnoreCase
        )

let readInput () =
    try
        System.Console.ReadLine().ToLower().Trim()
    with _ ->
        exit 0

let peekSome game =
    game |> Option.map (printfn "%O") |> ignore
    game


let readMove game =
    printf "Enter your move (row column): "
    let input = readInput ()

    if input.icompare "q" || input.icompare "quit" then
        printfn "Bye"
        exit 0

    let parts = input.Split(' ')

    if parts.Length <> 2 then
        Error("Invalid input. There should be two numbers", game)
    else
        Ok()
    |> Result.bind (fun _ ->
        try
            Ok(int parts.[0])
        with _ ->
            Error("Invalid input. Row should be a number.", game))
    |> Result.bind (fun row ->
        try
            Ok(row, int parts.[1])
        with _ ->
            Error("Invalid input. Column should be a number.", game))
    |> Result.bind (fun (row, col) ->
        if row < 0 || row > 2 || col < 0 || col > 2 then
            Error(
                "Invalid move. Please enter numbers between 0 and 2.",
                game
            )
        else
            Ok(row, col, game))

let rec gameLoop (game: Game option) =
    game
    |> peekSome
    |> readMove
    |> (function
    | Error(e, game) ->
        do printfn "%s" e |> ignore
        game
    | Ok(row, col, game) -> game |> Option.bind (turn row col))
    |> (fun g ->
        match g with
        | Some { State = (Playing _) } -> (gameLoop g)
        | Some { State = (Invalid(InvalidMove(row, col))) } ->
            do printfn "Invalid move %d %d" row col |> ignore
            gameLoop game
        | _ -> g)

let rec mainLoop (game: Game option) =
    game
    |> gameLoop
    |> peekSome
    |> (fun g ->
        match g with
        | Some { State = ste } ->

            printf "Play again?"

            let p =
                match ste with
                | Win(_, p) -> p
                | Drawn(_, p) -> otherPlayer p

            let (i: string) = readInput ()

            if i.icompare "yes" || i.icompare "y" then
                newGame (playerPiece p) |> Some |> mainLoop
            else
                printfn "Bye"
                exit 0
        | _ -> g)

[<EntryPoint>]
let main _ =
    printfn "\n:: T I C :: T A C ::\n"
    Some(newGame Piece.X) |> mainLoop |> ignore
    0

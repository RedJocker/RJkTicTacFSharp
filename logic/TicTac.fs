module TicTac

type Piece =
    | X
    | O

    override this.ToString() =
        match this with
        | X -> "X"
        | O -> "O"

type Square =
    | With of Piece
    | Empty

    override this.ToString() =
        match this with
        | With p -> sprintf "%O" p
        | Empty -> "_"

type Row =
    | Row of (Square * Square * Square)

    override this.ToString() =
        match this with
        | Row(s0, s1, s2) -> sprintf "| %O | %O | %O |" s0 s1 s2

type Board =
    | Board of (Row * Row * Row)

    override this.ToString() =
        match this with
        | Board(row0, row1, row2) ->
            sprintf "\n%O\n%O\n%O\n" row0 row1 row2

type Player1 = Player1 of Piece
type Player2 = Player2 of Piece

type Player =
    | P1 of Player1
    | P2 of Player2

    override this.ToString() =
        match this with
        | P1(Player1 p) -> sprintf "Player1 %O" p
        | P2(Player2 p) -> sprintf "Player2 %O" p

type Invalid =
    | InvalidState of Board
    | InvalidMove of int * int

type GameState =
    | Playing of (Board * Player)
    | Win of (Board * Player)
    | Drawn of (Board * Player)
    | Invalid of Invalid

    override this.ToString() =
        match this with
        | Playing(board, player) ->
            sprintf "Turn: %O\n%O" player board
        | Win(board, player) -> sprintf "%O\n%O Wins" board player
        | Drawn(board, player) -> sprintf "%O\nGame Drawn" board
        | Invalid(InvalidState board) ->
            sprintf "INVALID BOARD STATE\n%O" board
        | Invalid(InvalidMove(row, col)) ->
            sprintf "Invalid Move %d %d" row col

type Game =
    { Player1: Player1
      Player2: Player2
      State: GameState }

    override this.ToString() =
        match this with
        | { Player1 = p1
            Player2 = p2
            State = s } -> sprintf "%O Vs %O\n%O" p1 p2 s

let newBoard =
    let newRow = Row(Empty, Empty, Empty)
    Board(newRow, newRow, newRow)

let putPiece piece row col board =
    let putPieceRow bRow =
        match (bRow, col) with
        | (Row(p0, p1, p2), 0) ->
            if p0 = Empty then
                Row(With piece, p1, p2) |> Some
            else
                None
        | (Row(p0, p1, p2), 1) ->
            if p1 = Empty then
                Row(p0, With piece, p2) |> Some
            else
                None
        | (Row(p0, p1, p2), 2) ->
            if p2 = Empty then
                Row(p0, p1, With piece) |> Some
            else
                None
        | _ -> None

    match (board, row) with
    | (Board(row0, row1, row2), 0) ->
        putPieceRow row0
        |> Option.map (fun newRow -> Board(newRow, row1, row2))
    | (Board(row0, row1, row2), 1) ->
        putPieceRow row1
        |> Option.map (fun newRow -> Board(row0, newRow, row2))
    | (Board(row0, row1, row2), 2) ->
        putPieceRow row2
        |> Option.map (fun newRow -> Board(row0, row1, newRow))
    | _ -> None

let newGame startPiece =
    let startPlayer = Player1 startPiece

    { Player1 = startPlayer
      Player2 = Player2(if startPiece = X then O else X)
      State = Playing(newBoard, P1 startPlayer) }

let playerPiece (player: Player) =
    match player with
    | P1(Player1 piece) -> piece
    | P2(Player2 piece) -> piece

let otherPlayer player =
    match player with
    | P1(Player1 X) -> P2(Player2 O)
    | P1(Player1 O) -> P2(Player2 X)
    | P2(Player2 X) -> P1(Player1 O)
    | P2(Player2 O) -> P1(Player1 X)

let countPiece piece board =
    let countRow =
        function
        | Row(s0, s1, s2) ->
            [ s0; s1; s2 ]
            |> List.filter (fun s ->
                match s with
                | With p -> p = piece
                | _ -> false)
            |> List.length

    match board with
    | Board(row0, row1, row2) ->
        [ row0; row1; row2 ] |> List.map countRow |> List.sum

let hasVictory (board: Board) : bool option =
    let samePiece =
        function
        | (a, b, c) -> a <> Empty && a = b && a = c

    match board with
    | Board((Row(s0, s1, s2)), (Row(s3, s4, s5)), (Row(s6, s7, s8))) ->
        let winRows =
            [ (s0, s1, s2); (s3, s4, s5); (s6, s7, s8) ]
            |> List.filter samePiece
            |> List.length

        let winCols =
            [ (s0, s3, s6); (s1, s4, s7); (s2, s5, s8) ]
            |> List.filter samePiece
            |> List.length

        let winDiags =
            [ (s0, s4, s8); (s2, s4, s6) ]
            |> List.filter samePiece
            |> List.length

        match (winRows, winCols, winDiags) with
        | (0, 0, 0) -> Some false
        | (1, 1, 0) -> Some true
        | (1, 0, 1) -> Some true
        | (0, 1, 1) -> Some true
        | (0, 0, 2) -> Some true
        | (1, 0, 0) -> Some true
        | (0, 1, 0) -> Some true
        | (0, 0, 1) -> Some true
        | _ -> None

let checkState (board: Board) (player: Player) : GameState =
    let xCount = countPiece X board
    let oCount = countPiece O board

    match player with
    | P2(Player2 _) when xCount <> oCount ->
        Invalid(InvalidState board)
    | P1(Player1 _) when abs (xCount - oCount) <> 1 ->
        Invalid(InvalidState board)
    | _ ->
        match hasVictory board with
        | Some true -> Win(board, player)
        | Some false when xCount + oCount = 9 -> Drawn(board, player)
        | Some false -> Playing(board, (otherPlayer player))
        | None -> Invalid(InvalidState board)

let turn (row: int) (col: int) (game: Game) : Game option =
    match game with
    | { Player1 = p1
        Player2 = p2
        State = Playing(board, player) } ->

        let piece = playerPiece player

        (putPiece piece row col board)
        |> (function
        | Some b ->
            Some
                { game with
                    State = (checkState b player) }
        | None ->
            Some
                { game with
                    State = Invalid(InvalidMove(row, col)) })
    | _ -> Some game

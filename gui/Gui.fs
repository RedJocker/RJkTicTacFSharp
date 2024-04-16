namespace gui

open Avalonia.FuncUI.DSL

module Gui =
    open Avalonia.Controls
    open Avalonia.Layout
    open TicTac

    type State =
        { game: Game
          input: string
          isEnd: bool }

    let init _ =
        { game = newGame X
          input = ""
          isEnd = false }

    type Msg =
        | NewGame of Game
        | End of Game
        | Turn of Game
        | SetInput of string


    let update (msg: Msg) (state: State) : State =
        match msg with
        | Turn(game) -> { state with game = game; input = "" }
        | SetInput input -> { state with input = input }
        | End game ->
            { state with
                isEnd = true
                game = game
                input = "" }
        | NewGame game ->
            { state with
                game = game
                input = ""
                isEnd = false }

    let textChangedCallback
        (input: string)
        (state: State)
        (dispatch: Msg -> unit)
        =
        if state.isEnd then
            if System.String.IsNullOrWhiteSpace input then
                ()
            else
                match state.game with
                | { State = Drawn(_, p) } ->
                    newGame ((otherPlayer >> playerPiece) p)
                    |> NewGame
                    |> dispatch
                | { State = Win(_, p) } ->
                    newGame (playerPiece p) |> NewGame |> dispatch
                | _ -> ()
        else
            let updatedValue =
                try
                    Some(input.Trim().Split(' '))
                with _ ->
                    None
                |> Option.bind (fun parts ->
                    if parts.Length <> 2 then None else Some(parts))
                |> Option.bind (fun parts ->
                    try
                        Some(int parts.[0], parts)
                    with _ ->
                        None)
                |> Option.bind (fun (row, parts) ->
                    try
                        Some(row, int parts.[1])
                    with _ ->
                        None)
                |> Option.bind (fun (row, col) ->
                    if row < 0 || row > 2 || col < 0 || col > 2 then
                        None
                    else
                        Some(row, col))
                |> Option.bind (fun (row, col) -> turn row col state.game)

            match updatedValue with
            | Some game ->
                match game with
                | { State = Playing _ } -> game |> Turn |> dispatch
                | { State = Drawn(_, p) } -> game |> End |> dispatch
                | { State = Win(_, p) } -> game |> End |> dispatch
                | _ -> ()
            | _ -> string input |> SetInput |> dispatch


    let view (state: State) (dispatch) =
        DockPanel.create
            [ DockPanel.children
                  [ TextBox.create
                        [ TextBox.dock Dock.Bottom
                          TextBox.text (string state.input)
                          TextBox.onTextChanged (
                              (fun input ->
                                  textChangedCallback input state dispatch),
                              SubPatchOptions.OnChangeOf state.game
                          )

                          TextBox.horizontalAlignment
                              HorizontalAlignment.Stretch ]
                    TextBlock.create
                        [ TextBlock.dock Dock.Top
                          TextBlock.fontSize 48.0
                          TextBlock.verticalAlignment VerticalAlignment.Center
                          TextBlock.horizontalAlignment
                              HorizontalAlignment.Center
                          TextBlock.text (string state.game) ] ] ]

namespace gui

open Avalonia.FuncUI.DSL

module Gui =
    open Avalonia.Controls
    open Avalonia.Layout
    open TicTac


    type State = { game: Game; input: string }
    let init () = { game = newGame X; input = "" }

    type Msg =
        | Reset
        | SetInput of string
        | Turn of Game

    let update (msg: Msg) (state: State) : State =
        match msg with
        | Turn(game) ->
            { state with game = game; input = "" }
        | SetInput input -> { state with input = input }
        | Reset -> init ()

    let view (state: State) (dispatch) =
        let game = state.game


        DockPanel.create
            [ DockPanel.children
                  [ TextBox.create
                        [ TextBox.dock Dock.Bottom
                          TextBox.text (string state.input)
                          TextBox.onTextChanged (
                              (fun input ->
                                  let updatedValue =
                                      try
                                          Some(
                                              input
                                                  .Trim()
                                                  .Split(
                                                      ' '
                                                  )
                                          )
                                      with _ ->
                                          None
                                      |> Option.bind
                                          (fun parts ->
                                              if
                                                  parts.Length
                                                  <> 2
                                              then
                                                  None
                                              else
                                                  Some(
                                                      parts
                                                  ))
                                      |> Option.bind
                                          (fun parts ->
                                              try
                                                  Some(
                                                      int
                                                          parts.[0],
                                                      parts
                                                  )
                                              with _ ->
                                                  None)
                                      |> Option.bind
                                          (fun (row, parts) ->
                                              try
                                                  Some(
                                                      row,
                                                      int
                                                          parts.[1]
                                                  )
                                              with _ ->
                                                  None)
                                      |> Option.bind
                                          (fun (row, col) ->
                                              if
                                                  row < 0
                                                  || row > 2
                                                  || col < 0
                                                  || col > 2
                                              then
                                                  None
                                              else
                                                  Some(
                                                      row,
                                                      col
                                                  ))
                                      |> Option.bind
                                          (fun (row, col) ->
                                              turn
                                                  row
                                                  col
                                                  game)

                                  match updatedValue with
                                  | Some game ->
                                      match game with
                                      | { State = Playing _ } ->
                                          game
                                          |> Turn
                                          |> dispatch
                                      | { State = Drawn(_,
                                                        p) }
                                      | { State = Win(_, p) } ->
                                          newGame (
                                              playerPiece p
                                          )
                                          |> Turn
                                          |> dispatch
                                      | _ -> ()
                                  | _ ->

                                      string input
                                      |> SetInput
                                      |> dispatch

                              ),
                              SubPatchOptions.OnChangeOf
                                  state.game
                          )


                          TextBox.horizontalAlignment
                              HorizontalAlignment.Stretch ]
                    TextBlock.create
                        [ TextBlock.dock Dock.Top
                          TextBlock.fontSize 48.0
                          TextBlock.verticalAlignment
                              VerticalAlignment.Center
                          TextBlock.horizontalAlignment
                              HorizontalAlignment.Center
                          TextBlock.text (string state.game) ] ] ]

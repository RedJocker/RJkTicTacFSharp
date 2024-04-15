namespace gui

open Avalonia.FuncUI.DSL

module Gui =
    open Avalonia.Controls
    open Avalonia.Layout
    open TicTac


    type State = { game: Game }
    let init () = { game = newGame X }

    type Msg =
        | Reset
        | Turn of Game

    let update (msg: Msg) (state: State) : State =
        match msg with
        | Turn(game) -> { state with game = game }
        | Reset -> init ()

    let view (state: State) (dispatch) =
        let game = state.game
        printfn "AAAAA%O" game

        DockPanel.create
            [ DockPanel.children
                  [ TextBox.create
                        [ TextBox.dock Dock.Bottom
                          TextBox.onTextChanged (
                              (fun input ->
                                  let updatedValue =
                                      try
                                          Some(
                                              input.Split(
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
                                              printfn
                                                  "%O"
                                                  game

                                              turn
                                                  row
                                                  col
                                                  game)

                                  match updatedValue with
                                  | Some game ->
                                      match game with
                                      | { State = Playing _ }
                                      | { State = Drawn _ }
                                      | { State = Win _ } ->
                                          game
                                          |> Turn
                                          |> dispatch
                                      | _ -> ()
                                  | _ -> ()

                              ),
                              SubPatchOptions.OnChangeOf
                                  state.game
                          )
                          TextBox.text ("")
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

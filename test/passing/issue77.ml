let div =
  [ div
      ~a:
        [ Reactive.a_style
            (React.S.map (sprintf "height: %dpx")
               (State.player_height_signal app_state))
          (* ksprintf a_style "%s" (if_smth "min-height: 300px;" ""); *)
         ]
      content ]

[@@@ocamlformat "type-decl=sparse"]

type t =
  { x: int
  ; y: int }

type t =
  [ `X
  | `Y ]

[@@@ocamlformat "space-around-collection-expressions"]

type t =
  { x: int
  ; y: int
  }

type t =
  [ `X
  | `Y
  ]

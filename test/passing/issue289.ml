[@@@ocamlformat "wrap-fun-args=false"]

let foo =
  let open Gql in
  [ field
      "id"
      ~doc:"Toy ID."
      ~args:[]
      ~typ:(non_null guid)
      ~resolve:(function _ctx -> x.id )
  ; field "id" ~doc:"Toy ID." ~args:[] ~typppp ~resolve:(function _ctx ->
        x.id )
  ; field
      "id"
      ~doc:"Toy ID."
      ~args:[]
      ~typ:(non_null guid)
      ~resolve:(function
        | A -> x.id
        | B -> c )
  ; field "id" ~doc:"Toy ID." ~args:[] ~resolve:(function
        | A -> x.id
        | B -> c )
  ; field
      "id"
      ~doc:"Toy ID."
      ~args:[]
      ~typppppppppppppppppppp
      ~resolve:(function
        | AAAAAAAAAAAAAAAAAAAa -> x.idddddddddddddddddddddddddd
        | BBBBBBBBBBBBBBBB -> ccccccccccccccccccccccc )
  ; field "id" ~doc:"Toy ID." ~args:[] ~resolve:(function
        | AAAAAAAAAAAAAAAAAAAa -> x.idddddddddddddddddddddddddd
        | BBBBBBBBBBBBBBBB -> ccccccccccccccccccccccc )
  ; field
      "id"
      ~doc:"Toy ID."
      ~args:[]
      ~typ:(non_null guid)
      ~resolve:(fun _ctx x -> x.id)
  ; field
      "name"
      ~doc:"Toy name."
      ~args:[]
      ~typ:(non_null string)
      ~resolve:(fun _ctx x -> x.name)
  ; field
      "description"
      ~doc:"Toy description."
      ~args:[]
      ~typ:string
      ~resolve:(fun _ctx x -> x.description |> Util.option_of_string)
  ; field
      "type"
      ~doc:"Toy type. Possible values are: car, animal, train."
      ~args:[]
      ~typ:(non_null toy_type_enum)
      ~resolve:(fun _ctx x -> x.toy_type)
  ; field
      "createdAt"
      ~doc:"Date created."
      ~args:[]
      ~typ:(non_null Scalar.date_time)
      ~resolve:(fun _ctx x -> x.created_at) ]

[@@@ocamlformat "wrap-fun-args=true"]

let foo =
  let open Gql in
  [ field "id" ~doc:"Toy ID." ~args:[] ~typ:(non_null guid)
        ~resolve:(function _ctx -> x.id )
  ; field "id" ~doc:"Toy ID." ~args:[] ~typppp ~resolve:(function _ctx ->
        x.id )
  ; field "id" ~doc:"Toy ID." ~args:[] ~typ:(non_null guid)
      ~resolve:(function
      | A -> x.id
      | B -> c )
  ; field "id" ~doc:"Toy ID." ~args:[] ~resolve:(function
      | A -> x.id
      | B -> c )
  ; field "id" ~doc:"Toy ID." ~args:[] ~typppppppppppppppppppp
      ~resolve:(function
      | AAAAAAAAAAAAAAAAAAAa -> x.idddddddddddddddddddddddddd
      | BBBBBBBBBBBBBBBB -> ccccccccccccccccccccccc )
  ; field "id" ~doc:"Toy ID." ~args:[] ~resolve:(function
      | AAAAAAAAAAAAAAAAAAAa -> x.idddddddddddddddddddddddddd
      | BBBBBBBBBBBBBBBB -> ccccccccccccccccccccccc )
  ; field "id" ~doc:"Toy ID." ~args:[] ~typ:(non_null guid)
      ~resolve:(fun _ctx x -> x.id)
  ; field "name" ~doc:"Toy name." ~args:[] ~typ:(non_null string)
      ~resolve:(fun _ctx x -> x.name)
  ; field "description" ~doc:"Toy description." ~args:[] ~typ:string
      ~resolve:(fun _ctx x -> x.description |> Util.option_of_string)
  ; field "type" ~doc:"Toy type. Possible values are: car, animal, train."
      ~args:[] ~typ:(non_null toy_type_enum) ~resolve:(fun _ctx x ->
        x.toy_type)
  ; field "createdAt" ~doc:"Date created." ~args:[]
      ~typ:(non_null Scalar.date_time) ~resolve:(fun _ctx x -> x.created_at)
  ]

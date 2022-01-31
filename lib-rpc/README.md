# ocamlformat-rpc-lib

The whole API is functorized over an `IO` module defining the blocking interface for reading and writing the data. This module has the following interface:

```ocaml
  type 'a t (** ['a t] represents a blocking monad state *)
  type ic (** [ic] represents an input channel *)
  type oc (** [oc] represents an output channel *)

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  (** [a >>= b] will pass the result of [a] to the [b] function. This is a
      monadic [bind]. *)

  val return : 'a -> 'a t
  (** [return a] will construct a constant IO value. *)

  val read : ic -> Csexp.t option t
  val write : oc -> Csexp.t list -> unit t
```

A basic implementation of this module can be:
```ocaml
module IO = struct
  type 'a t = 'a
  type ic = in_channel
  type oc = out_channel

  let ( >>= ) x f = f x
  let return x = x

  let read ic =
    match Csexp.input ic with
    | Ok x -> return (Some x)
    | Error _ -> return None

  let write oc lx =
    List.iter (Csexp.to_channel oc) lx ;
    Stdlib.flush oc ;
    return ()
end
```

After you decided on the `IO` implementation, the `Ocamlformat_rpc_lib` API can then be instantiated:
```ocaml
module RPC = Ocamlformat_rpc_lib.Make (IO)
```

The RPC offers multiple versions of the API, the easiest way to set up a server and decide on a version is to use the `pick_client` function, which is parameterized by a process id, an input channel, an output channel and the list of versions you are willing to use:
```ocaml
val pick_client :
       pid:int
    -> IO.ic
    -> IO.oc
    -> string list
    -> (client, [`Msg of string]) result IO.t
```
`pick_client` will handle the interaction with the server to agree with a common version of the RPC to use. Trying successively each version of the provided list until the server agrees, for this reason you might want to list newer versions before older versions.

Once a client is returned the following functions are available to the user:
```ocaml
val pid : client -> int

val halt : client -> (unit, [> `Msg of string]) result IO.t

val config :
       (string * string) list
    -> client
    -> (unit, [> `Msg of string]) result IO.t

val format :
       ?format_args:format_args
    -> string
    -> client
    -> (string, [> `Msg of string]) result IO.t
```

A basic interaction could be:
```ocaml
RPC.config [("profile", "ocamlformat")] client >>= fun () ->
RPC.format "let x = 4 in x" client >>= fun formatted ->
...
RPC.halt client >>= fun () ->
...
```

For a full working example, see: [test/rpc/rpc_test.ml](../test/rpc/rpc_test.ml).

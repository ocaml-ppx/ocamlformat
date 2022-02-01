# ocamlformat-rpc

`ocamlformat-rpc` is a RPC mode for OCamlFormat, a tool to format OCaml code.

`ocamlformat-rpc` listens to RPC requests, provided on the standard input, and prints the response on the standard output.

## Agreeing on a RPC version

Before the client and the server agree on a common version to use the following commands are available:
- `Halt` to close the connection to the RPC;
- `Version v` to ask the server to use version `v`.

If the server agrees upon the version he will send the reply `Version v` and the protocol version is set to `v`, to use another version later the client has to close the connexion and start a new one. If the server cannot use version `v` he might propose another version `w` by sending the reply `Version w` that the client can accept by sending the same request for version `w`, or propose another version. If the server cannot propose another version it will close the connection. Unknown commands are ignored.

Once the client and the server agree on a common version, the requests you can send may differ from one version to another.

## Commands

All versions support the following commands:
- `Halt` to end the communication with the RPC server. The caller must close the input and output channels.

Some RPC versions offer specific commands, that are detailed below.

### Version 1

Specific commands supported on version v1 are:
- `Config <csexp>`: submits a list of (key, value) pairs (as a canonical s-expression) to update OCamlFormat's configuration (please refer to `ocamlformat --help` to know more about the available options). The accepted configuration is sent as a reply of the same form. The configuration can be reset to its default value by sending the pair `("profile", "default").
- `Format <csexp>`: submits a fragment of OCaml code (as a canonical s-expression) to be formatted by OCamlFormat, the formatted output is sent as a reply of the same form `Format <csexp>`"

### Version 2

Specific commands supported on version v2 are:
- `Format <csexp>`: submits a list as canonical s-expression, where the first element of the list is a string to be formatted by OCamlFormat. The other arguments are (key, value) pairs, where key can be either `"Path"` and/or `"Config"`. They modify the server's configuration temporarily, for the current request. The formatted output is sent as a reply of the same form.

Unknown commands are ignored.

To build your RPC client to interact with `ocamlformat-rpc` please refer to `ocamlformat-rpc-lib`'s [documentation](../../lib-rpc/README.md).
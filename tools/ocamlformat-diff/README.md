# OCamlFormat diff tool

`ocamlformat_diff` is a tool that uses OCamlFormat to apply the same formatting to compared OCaml files, so that the formatting differences between the two files are not displayed.

The file comparison is then performed by any `diff` backend.


## Options

The options' documentation is available in through `ocamlformat_diff --help`.

The option `--default-diff` allows you to configure the diff command that is used to compare the formatted files. The default value is the vanilla `diff`, but you can also use [patdiff](https://github.com/janestreet/patdiff) or any other similar comparison tool.


## Git integration

To call `ocamlformat_diff` with `git diff`, yYou have to edit your `.git` configuration files on each project you want to use `ocamlformat_diff` with.

The `.git/info/attributes` file must contain:
```
*.ml diff=ocamlformat_diff
*.mli diff=ocamlformat_diff
*.mlt diff=ocamlformat_diff
```

The `.git/config` file must contain:

```
[diff "ocamlformat_diff"]
    command = ocamlformat_diff
```

if you want to use the default `diff` command.

Or:

```
[diff "ocamlformat_diff"]
    command = ocamlformat_diff --default-diff=patdiff
```
if you want to use [patdiff](https://github.com/janestreet/patdiff).
Feel free to use another diff command as you see fit.

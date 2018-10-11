# OCamlFormat diff tool

`ocamlformat-diff` is a tool that uses OCamlFormat to apply the same formatting to compared OCaml files, so that the formatting differences between the two files are not displayed.

The file comparison is then performed by any `diff` backend.


## Options

The options' documentation is available through `ocamlformat-diff --help`.

The option `--diff` allows you to configure the diff command that is used to compare the formatted files. The default value is the vanilla `diff`, but you can also use [patdiff](https://github.com/janestreet/patdiff) or any other similar comparison tool.


## Git integration

To call `ocamlformat-diff` with `git diff`, you have to edit your `.git` configuration files on each project you want to use `ocamlformat-diff` with.

The `.git/info/attributes` file must contain:
```
*.ml diff=ocamlformat-diff
*.mli diff=ocamlformat-diff
*.mlt diff=ocamlformat-diff
```

The `.git/config` file must contain:

```
[diff "ocamlformat-diff"]
    command = ocamlformat-diff --diff=patdiff
```

Here [patdiff](https://github.com/janestreet/patdiff) will be used to compare OCaml files, you can also omit the `--diff` option to use the vanilla `diff` command.

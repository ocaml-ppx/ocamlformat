open B0_kit.V000
open Result.Syntax

let unicode_version = 15, 1, 0, None (* Adjust on new releases *)
let next_major = let maj, _, _, _ = unicode_version in (maj + 1), 0, 0, None

(* OCaml library names *)

let unix = B0_ocaml.libname "unix"
let uucd = B0_ocaml.libname "uucd"
let uunf = B0_ocaml.libname "uunf"
let cmdliner = B0_ocaml.libname "cmdliner"

let uucp = B0_ocaml.libname "uucp"

(* Libraries *)

let uucp_lib = B0_ocaml.lib uucp ~doc:"The uucp library" ~srcs:[ `Dir ~/"src" ]

(* Data generation. *)

let generate_data =
  let doc = "uucp_*_data.ml files generator" in
  let srcs =
    [ `Dir ~/"support";
      (* Well that was based on ocamlbuild loose' inclusion,
         maybe we could move all that to a single _base.ml module. *)
      `File ~/"src/uucp_block_base.ml";
      `File ~/"src/uucp_break_base.ml";
      `File ~/"src/uucp_gc_base.ml";
      `File ~/"src/uucp_hangul_base.ml";
      `File ~/"src/uucp_name_base.ml";
      `File ~/"src/uucp_num_base.ml";
      `File ~/"src/uucp_script_base.ml";
      (* *)
      `File ~/"src/uucp_cmap.ml";
      `File ~/"src/uucp_fmt.ml";
      `File ~/"src/uucp_rmap.ml";
      `File ~/"src/uucp_tmap.ml";
      `File ~/"src/uucp_tmap5bytes.ml";
      `File ~/"src/uucp_tmapbool.ml";
      `File ~/"src/uucp_tmapbyte.ml"; ]
  in
  let requires = [ uucd; unix ] in
  let meta =
    B0_meta.empty
    |> B0_meta.(tag build)
    |> B0_meta.add B0_unit.exec_cwd `Scope_dir
  in
  B0_ocaml.exe "generate-data" ~doc ~srcs ~requires ~meta

(* Tools *)

let ucharinfo =
  let doc = "The ucharinfo tool" in
  let srcs = [ `File ~/"test/ucharinfo.ml" ] in
  let requires = [ cmdliner; uunf; uucp ] in
  B0_ocaml.exe "ucharinfo" ~public:true ~doc ~srcs ~requires

(* Tests *)

let test ?(meta = B0_meta.empty) ?(requires = [ uucp ]) name ~src ~doc =
  let srcs = [ `File ~/src ] in
  let meta = B0_meta.(tag test) meta in
  B0_ocaml.exe name ~doc ~srcs ~meta ~requires

let test' =
  let doc = "Test Uucp against the Unicode database." in
  let meta = B0_meta.empty |> B0_meta.add B0_unit.exec_cwd `Scope_dir in
  test "test" ~requires:[ uucd; uucp ] ~src:"test/test.ml" ~doc ~meta

let perf =
  test "perf" ~requires:[ uucp ] ~src:"test/perf.ml" ~doc:"Test performance"

let link_test =
  test "link_test" ~requires:[ uucp ] ~src:"test/link_test.ml" ~doc:"Link test"

let examples =
  let doc = "Doc samples" in
  test "examples" ~requires:[ uucp; uunf ] ~src:"test/examples.ml" ~doc

(* Actions *)

let uc_base = "http://www.unicode.org/Public"

let unzip env = B0_env.get_tool env (Cmd.arg "unzip")
let curl env =
  B0_env.get_tool env @@
  Cmd.(arg "curl" % "--fail" % "--show-error" % "--progress-bar" % "--location")

let show_version =
  B0_action.make' "unicode-version" ~doc:"Show supported unicode version" @@
  fun _ _ ~args:_ ->
  Ok (Log.app (fun m -> m "%s" (String.of_version unicode_version)))

let download_ucdxml =
  B0_action.make' "download-ucdxml" ~doc:"Download the ucdxml" @@
  fun _ env ~args:_ ->
  let* curl = curl env and* unzip = unzip env in
  let version = String.of_version unicode_version in
  let ucd_uri = Fmt.str "%s/%s/ucdxml/ucd.all.grouped.zip" uc_base version in
  let ucd_file = Fpath.v "support/ucd.xml" in
  let ucd_file = B0_env.in_scope_dir env ucd_file in
  Result.join @@ Os.File.with_tmp_fd @@ fun tmpfile tmpfd ->
  (Log.app @@ fun m ->
   m "@[<v>Downloading %s@,to %a@]" ucd_uri Fpath.pp ucd_file);
  let* () =
    let stdout = Os.Cmd.out_fd ~close:true tmpfd in
    Os.Cmd.run Cmd.(curl % ucd_uri) ~stdout
  in
  let* () =
    let stdout = Os.Cmd.out_file ~force:true ~make_path:true ucd_file in
    Os.Cmd.run Cmd.(unzip % "-p" %% path tmpfile) ~stdout
  in
  Ok ()

(* Packs *)

let default =
  let unicode_version = String.of_version unicode_version in
  let next_major = String.of_version next_major in
  let meta =
    B0_meta.empty
    |> B0_meta.(add authors) ["The uucp programmers"]
    |> B0_meta.(add maintainers)
       ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> B0_meta.(add homepage) "https://erratique.ch/software/uucp"
    |> B0_meta.(add online_doc) "https://erratique.ch/software/uucp/doc/"
    |> B0_meta.(add licenses) ["ISC"]
    |> B0_meta.(add repo) "git+https://erratique.ch/repos/uucp.git"
    |> B0_meta.(add issues) "https://github.com/dbuenzli/uucp/issues"
    |> B0_meta.(add description_tags)
      ["unicode"; "text"; "character"; "org:erratique"]
    |> B0_meta.add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
         "--with-uunf" "%{uunf:installed}%"
         "--with-cmdliner" "%{cmdliner:installed}%" ]]|}
    |> B0_meta.tag B0_opam.tag
    |> B0_meta.add B0_opam.Meta.depopts [ "uunf", ""; "cmdliner", ""]
    |> B0_meta.add B0_opam.Meta.conflicts
      [ "uunf", Fmt.str {|< "%s" | >= "%s" |} unicode_version next_major;
        "cmdliner", {|< "1.1.0"|} ]
    |> B0_meta.add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.14.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
        "uucd", Fmt.str {|with-test dev & >= "%s" & < "%s"|}
          unicode_version next_major;
        "uunf", {|with-test|} ]
    |> B0_meta.add B0_opam.Meta.file_addendum
      [ `Field ("post-messages", `L (true, [
            `S "If the build fails with \"ocamlopt.opt got signal and \
                exited\", issue 'ulimit -s unlimited' and retry.";
            `Raw {|{failure & (arch = "ppc64" | arch = "arm64")}|}]))]
  in
  B0_pack.make "default" ~doc:"uucd package" ~meta ~locked:true @@
  B0_unit.list ()

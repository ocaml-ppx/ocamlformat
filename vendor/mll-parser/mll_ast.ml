type 'a located = {value: 'a; loc: Location.t}

type ocaml_code = string located

(** Character class entry (inside [...]) *)
type char_entry =
  | Cchar of string  (** single char literal, e.g. 'a' *)
  | Crange of string * string  (** range, e.g. 'a'-'z' *)

(** Regular expression *)
type regexp =
  | Reof
  | Rchar of string  (** character literal 'c' *)
  | Rstring of string  (** string literal "s" *)
  | Rident of string  (** named regexp reference *)
  | Runderscore  (** _ (any character) *)
  | Rsequence of regexp list
  | Ralternative of regexp list
  | Rrepeat of regexp  (** e* *)
  | Rplus of regexp  (** e+ *)
  | Roption of regexp  (** e? *)
  | Rparen of regexp  (** (e) *)
  | Rcharset of char_entry list  (** [set] *)
  | Rcharset_neg of char_entry list  (** [^set] *)
  | Ras of regexp * string  (** e as id *)
  | Rhash of regexp * regexp  (** e1 # e2 *)

type action = ocaml_code

type rule_case = {pattern: regexp; action: action}

type rule_entry =
  { entry_name: string located
  ; entry_args: string located list
  ; entry_is_shortest: bool
  ; entry_cases: rule_case list }

type named_def =
  { def_name: string located
  ; def_body: regexp
  ; def_loc: Location.t }

type lexer_def =
  { header: ocaml_code option
  ; named_defs: named_def list
  ; rules: rule_entry list
  ; trailer: ocaml_code option
  ; comments: ocaml_code list }

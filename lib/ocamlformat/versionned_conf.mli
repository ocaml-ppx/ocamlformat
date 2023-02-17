type mversion = (module Ocamlformat_version.S)

type t = {version: Version.t; version_module: mversion; conf: Conf.t}

val latest : mversion

val oldest : mversion

val m_of_version : Version.t -> (mversion, Translation_unit_error.t) Result.t

val mversion_of_string : string -> mversion option

val default : t

val parse_line :
     t
  -> ?version_check:bool
  -> ?disable_conf_attrs:bool
  -> from:[< `Attribute of Warnings.loc | `File of Warnings.loc > `File]
  -> string
  -> (t, Conf.Error.t) Result.t

val update : ?quiet:bool -> t -> Parsetree.attribute -> t
(** [update ?quiet c a] updates configuration [c] after reading attribute
    [a]. [quiet] is false by default. *)

val term : (t -> t) Cmdliner.Term.t

val map_conf : f:(Conf.t -> Conf.t) -> t -> t
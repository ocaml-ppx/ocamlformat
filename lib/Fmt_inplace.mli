type fmt_code =
     Conf.t
  -> offset:int
  -> set_margin:bool
  -> string
  -> (Fmt.t, [`Msg of string]) Result.t

type block = {value: string; loc: Location.t; block_level: bool}

val format_inplace :
     source:Source.t
  -> fmt_code:fmt_code
  -> fmt_code_structure:fmt_code
  -> conf:Conf.t
  -> menhir_mode:bool
  -> cmts:Cmts.t
  -> block list
  -> string

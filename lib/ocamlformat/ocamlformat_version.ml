module type S = sig
  module Translation_unit : sig
    val parse_and_format :
         Syntax.t
      -> ?output_file:string
      -> input_name:string
      -> source:string
      -> Conf.t
      -> (string, Translation_unit_error.t) Result.t
    (** [parse_and_format kind ?output_file ~input_name ~source conf opts]
        parses and formats [source] as a list of fragments. *)

    val numeric :
         Syntax.t
      -> input_name:string
      -> source:string
      -> range:Range.t
      -> Conf.t
      -> int list
    (** [numeric ~input_name ~source ~range conf opts] returns the
        indentation of the range of lines [range] (line numbers ranging from
        1 to number of lines), where the line numbers are relative to
        [source] and the indentation is relative to the formatted output. *)

    val print_error :
         ?debug:bool
      -> ?quiet:bool
      -> Format.formatter
      -> Translation_unit_error.t
      -> unit
  end
end

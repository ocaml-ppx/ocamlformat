val with_warning_filter :
  filter_warning:(Location.t -> Warnings.t -> bool) -> filter_alert:(Location.t -> Warnings.alert -> bool) -> f:(unit -> 'a) -> 'a

val print_warning : Location.t -> Warnings.t -> unit

val is_unexpected_docstring : Warnings.t -> bool

val is_deprecated_alert : Warnings.alert -> bool

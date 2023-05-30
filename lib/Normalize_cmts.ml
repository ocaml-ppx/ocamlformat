module Normalized_cmt = struct
  type t =
    { cmt_kind: [`Comment | `Doc_comment]
    ; norm: string
    ; orig: Cmt.t  (** Not compared. *) }

  let compare a b = Poly.compare (a.cmt_kind, a.norm) (b.cmt_kind, b.norm)

  let of_cmt ~normalize_code ~normalize_doc orig =
    let cmt_kind, norm =
      let decoded = Cmt.decode orig in
      match decoded.Cmt.kind with
      | Verbatim txt -> (`Comment, txt)
      | Doc txt -> (`Doc_comment, normalize_doc txt)
      | Normal txt -> (`Comment, Docstring.normalize_text txt)
      | Code code -> (`Comment, normalize_code (String.concat ~sep:"\n" code))
      | Asterisk_prefixed lines ->
          ( `Comment
          , String.concat ~sep:" "
              (List.map ~f:Docstring.normalize_text lines) )
    in
    {cmt_kind; norm; orig}

  let dropped {cmt_kind; orig; _} = {Cmt.kind= `Dropped orig; cmt_kind}

  let added {cmt_kind; orig; _} = {Cmt.kind= `Added orig; cmt_kind}

  let sexp_of_t _ = Sexp.Atom "Normalized_cmt.t"

  module Comparator = struct
    type nonrec t = t

    include Comparator.Make (struct
      type nonrec t = t

      let compare, sexp_of_t = (compare, sexp_of_t)
    end)
  end
end

let diff ~f x y =
  (*= [symmetric_diff x y] returns a sequence of changes between [x] and [y]:
      - [First k] means [k] is in [x] but not [y]
      - [Second k] means [k] is in [y] but not [x] *)
  Set.symmetric_diff (f x) (f y)
  |> Sequence.to_list
  (*= - [First _] is reported as a comment dropped
      - [Second _] is reported as a comment added *)
  |> List.map
       ~f:
         (Either.value_map ~first:Normalized_cmt.dropped
            ~second:Normalized_cmt.added )
  |> function [] -> Ok () | errors -> Error errors

let normalize ~normalize_code ~parse_docstrings =
  object (self)
    method cmt c =
      Normalized_cmt.of_cmt ~normalize_code:self#code ~normalize_doc:self#doc
        c

    method cmts cs =
      List.map ~f:(fun c -> (self#cmt c).Normalized_cmt.norm) cs

    method code c = normalize_code ~normalize_cmts:self#cmts c

    method doc d =
      Docstring.normalize ~parse_docstrings ~normalize_code:self#code d
  end

let diff_cmts ~normalize_code ~parse_docstrings x y =
  let n = normalize ~normalize_code ~parse_docstrings in
  let f cmts =
    Set.of_list (module Normalized_cmt.Comparator) (List.map ~f:n#cmt cmts)
  in
  diff ~f x y

let normalize_docstring ~normalize_code ~parse_docstrings doc =
  let n = normalize ~normalize_code ~parse_docstrings in
  n#doc doc

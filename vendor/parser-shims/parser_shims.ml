module List = struct
  include List

  let rec find_map f = function
    | [] -> None
    | x :: l ->
        begin match f x with
        | Some _ as result -> result
        | None -> find_map f l
        end
end

module Misc = struct
  include Misc

  module Color = struct
    include Color

    let default_setting = Auto
  end

  module Error_style = struct
    include Error_style

    let default_setting = Contextual
  end
end

module List = List
module Warning = Warning

module Parser : sig
  include module type of Parser with type token = Token_latest.token
  include module type of Token_latest
end

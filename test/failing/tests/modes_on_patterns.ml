(* Modes on arbitrary patterns were supported in the parser during development of
   ocamlformat support for modes, but were later unsupported in the parser, causing
   the below tests to fail. If patterns are ever again supported in the parser, move
   these tests back to [test/passing/modes*.ml]. *)

module Patterns = struct
  (* [let (pat @ mode) = x] parses as a mode on the let binding, not on the pattern *)
  let pat @ mode = x
  let (pat : typ @@ mode) = x

  (* [let (pat @ mode), (pat @ mode) = x] currently does not parse *)
  let ((pat @ mode), (pat @ mode)) = x

  let () =
    let ((pat @ mode), (pat @ mode)) = x in
    ()
  ;;

  let { a = (a @ mode); b = (b1 @ mode), (b2 @ mode); c = (c : typ @@ mode) } = x
  let (A ((a @ mode), (b @ mode))) = x
  let (A (a @ mode) @ mode) = x

  (* mode constraints in other patterns *)
  let { alias1 = (x @ mode) as y
      ; alias2 = (x : typ @@ mode) as y
      ; tuple1 = (x @ mode), (y @ mode)
      ; tuple2 = (x : typ @@ mode), (y : typ @@ mode)
      ; tuple3 = ~x:(x @ mode), ~y:(y @ mode)
      ; tuple4 = ~x:(x : typ @@ mode), ~y:(y : typ @@ mode)
      ; construct1 = A (x @ mode)
      ; construct2 = A ((x, y) @ mode)
      ; construct3 = A ((x @ mode), (y @ mode))
      ; construct4 = A (x : typ @@ mode)
      ; construct5 = A ((x, y) : typ @@ mode)
      ; construct6 = A ((x : typ @@ mode), (y : typ @@ mode))
      ; variant1 = `A (x @ mode)
      ; variant2 = `A ((x, y) @ mode)
      ; variant3 = `A ((x @ mode), (y @ mode))
      ; variant4 = `A (x : typ @@ mode)
      ; variant5 = `A ((x, y) : typ @@ mode)
      ; variant6 = `A ((x : typ @@ mode), (y : typ @@ mode))
      ; array1 = [| (x @ mode) |]
      ; array2 = [| (x : typ @@ mode) |]
      ; list1 = [ (x @ mode) ]
      ; list2 = [ (x : typ @@ mode) ]
      ; or1 = (x @ mode) | (y @ mode)
      ; or2 = (x : typ @@ mode) | (y : typ @@ mode)
      ; constraint1 = ((x @ mode) @ mode)
      ; constraint2 = ((x : typ @@ mode) @ mode)
      ; constraint3 = ((x @ mode) : typ @@ mode)
      ; constraint4 = ((x : typ @@ mode) : typ @@ mode)
      ; lazy1 = (lazy (x @ mode))
      ; lazy2 = (lazy (x : typ @@ mode))
      ; exception1 = (exception (x @ mode))
      ; exception2 = (exception (x : typ @@ mode))
      ; extension1 = [%ext? (x @ mode)]
      ; extension2 = [%ext? (x : typ @@ mode)]
      ; open1 = M.((X @ mode))
      ; open2 = M.((X : typ @@ mode))
      ; cons1 = (x @ mode) :: (y @ mode) :: (z @ mode)
      ; cons2 = (x : typ @@ mode) :: (y : typ @@ mode) :: (z : typ @@ mode)
      }
    =
    x
  ;;

  (* other patterns in mode constraints *)
  let { any1 = (_ @ mode)
      ; any2 = (_ : typ @@ mode)
      ; var1 = (x @ mode)
      ; var2 = (x : typ @@ mode)
      ; alias1 = (A as x @ mode)
      ; alias2 = (A as x : typ @@ mode)
      ; constant1 = ("" @ mode)
      ; constant2 = ("" : typ @@ mode)
      ; interval1 = ('a' .. 'z' @ mode)
      ; interval2 = ('a' .. 'z' : typ @@ mode)
      ; tuple1 = ((x, y) @ mode)
      ; tuple2 = ((x, y) : typ @@ mode)
      ; tuple3 = ((~x, ~y) @ mode)
      ; tuple4 = ((~x, ~y) : typ @@ mode)
      ; construct1 = (A @ mode)
      ; construct2 = (A x @ mode)
      ; construct3 = (A (x, y) @ mode)
      ; construct4 = (A { x; y } @ mode)
      ; construct5 = (A : typ @@ mode)
      ; construct6 = (A x : typ @@ mode)
      ; construct7 = (A (x, y) : typ @@ mode)
      ; construct8 = (A { x; y } : typ @@ mode)
      ; variant1 = (`A @ mode)
      ; variant2 = (`A x @ mode)
      ; variant3 = (`A (x, y) @ mode)
      ; variant4 = (`A : typ @@ mode)
      ; variant5 = (`A x : typ @@ mode)
      ; variant6 = (`A (x, y) : typ @@ mode)
      ; record1 = ({ x } @ mode)
      ; record2 = ({ x } : typ @@ mode)
      ; array1 = ([| x |] @ mode)
      ; array2 = ([| x |] : typ @@ mode)
      ; list1 = ([ x ] @ mode)
      ; list2 = ([ x ] : typ @@ mode)
      ; or1 = (x | y @ mode)
      ; or2 = (x | y : typ @@ mode)
      ; constraint1 = ((x @ mode) @ mode)
      ; constraint2 = ((x : typ @@ mode) @ mode)
      ; constraint3 = ((x @ mode) : typ @@ mode)
      ; constraint4 = ((x : typ @@ mode) : typ @@ mode)
      ; type1 = (#x @ mode)
      ; type2 = (#x : typ @@ mode)
      ; lazy1 = ((lazy x) @ mode)
      ; lazy2 = ((lazy x) : typ @@ mode)
      ; unpack1 = ((module P) @ mode)
      ; unpack2 = ((module P) : typ @@ mode)
      ; unpack3 = ((module P : S) @ mode)
      ; unpack4 = ((module P : S) : typ @@ mode)
      ; exception1 = ((exception E) @ mode)
      ; exception2 = ((exception E) : typ @@ mode)
      ; extension1 = ([%ext] @ mode)
      ; extension2 = ([%ext] : typ @@ mode)
      ; open1 = (M.(X x) @ mode)
      ; open2 = (M.(X x) : typ @@ mode)
      ; cons1 = (a :: b :: c @ mode)
      ; cons2 = (a :: b :: c : typ @@ mode)
      }
    =
    x
  ;;
end

module No_illegal_sugaring = struct
  let { x = (x : t @@ mode) } = y

  let () =
    let ((module M) : (module T) @@ mode) = ((module M) : (module T) @@ mode) in
    ()
  ;;

  let (~x:(x @ mode), ~y:(y @ mode)) = ~x:(x : _ @@ mode), ~y:(y : _ @@ mode)
end

module Line_breaking = struct
  module Patterns = struct
    let long_pat_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
        @ mode1 mode2 mode3 mode4 mode5 mode6 mode7 mode8
      =
      x
    ;;

    let (long_pat_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa :
           t
           @@ mode1 mode2 mode3 mode4 mode5 mode6 mode7 mode8)
      =
      x
    ;;

    let (long_pat_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa :
           long_type_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
           @@ mode1 mode2 mode3 mode4 mode5 mode6 mode7 mode8)
      =
      x
    ;;
  end
end

module Regressions = struct
  class t =
    let ((x @ mode), (y @ mode)) = x in
    object end
end

module Attrs = struct
  let (pat [@attr]) @ mode = x
  let ((pat [@attr]) : typ @@ mode) = x
  let (pat : (typ[@attr]) @@ mode) = x
  let ((pat : typ @@ mode) [@attr]) = x
end

module Comments = struct
  let (* cmt *) pat @ mode = x
  let pat (* cmt *) @ mode = x
  let pat @ (* cmt *) mode = x
  let pat @ mode (* cmt *) = x
  let (* cmt *) (pat : typ @@ mode) = x
  let ((* cmt *) pat : typ @@ mode) = x
  let (pat (* cmt *) : typ @@ mode) = x
  let (pat : (* cmt *) typ @@ mode) = x
  let (pat : typ (* cmt *) @@ mode) = x
  let (pat : typ @@ (* cmt *) mode) = x
  let (pat : typ @@ mode (* cmt *)) = x
  let (pat : typ @@ mode) (* cmt *) = x
end

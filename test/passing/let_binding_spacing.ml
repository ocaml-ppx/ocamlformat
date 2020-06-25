let f x = x

and g : 'a. (_ -> _ -> _ -> 'a) -> _ -> _ -> _ -> 'a =
 fun h a b -> h (i a) (i b) (i c)

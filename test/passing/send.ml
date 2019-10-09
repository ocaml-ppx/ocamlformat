let x obj = obj#hello ()

let x obj_f = (obj_f ())#hello ()

let f obj = obj#hello_some_pretty_long_one ~with_labels:true ()

let f obj =
  obj#hello_some_pretty_long_one ~with_labels:true "desjd\ndijsde\n"
    {md|
In **markdown**
|md}

let should_erase_ref = ref false

let set_should_erase yn = should_erase_ref := yn

let should_erase () = !should_erase_ref

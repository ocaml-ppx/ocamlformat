* Adapt for module aliases. The painful part is that at the moment it's 
  not possible to write eg `module Age : sig ... end = Uucp_age`. This means
  that in contrast to what is now done in the library `mli`s have to be 
  written for each `Uucp_*` and the corresponding `cmi`s have to be 
  installed.
  

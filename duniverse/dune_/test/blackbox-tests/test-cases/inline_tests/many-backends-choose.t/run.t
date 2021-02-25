  $ cat >dune-project <<EOF
  > (lang dune 2.6)
  > EOF
  $ cat >dune <<EOF
  > (library
  >  (name backend_mbc1)
  >  (modules ())
  >  (inline_tests.backend
  >   (generate_runner (echo "print_endline \"backend_mbc1\""))))
  > 
  > (library
  >  (name backend_mbc2)
  >  (modules ())
  >  (inline_tests.backend
  >   (generate_runner (echo "print_endline \"backend_mbc2\""))))
  > 
  > (library
  >  (name foo_mbc)
  >  (inline_tests (backend backend_mbc1))
  >  (libraries backend_mbc1 backend_mbc2))
  > EOF

  $ dune runtest
  inline_test_runner_foo_mbc alias runtest
  backend_mbc1

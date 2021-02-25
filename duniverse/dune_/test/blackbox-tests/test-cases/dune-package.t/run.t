  $ dune build
  $ dune_cmd cat _build/install/default/lib/a/dune-package | sed "s/(lang dune .*)/(lang dune <version>)/" | dune_cmd sanitize
  (lang dune <version>)
  (name a)
  (library
   (name a)
   (kind normal)
   (archives (byte a.cma) (native a.cmxa))
   (plugins (byte a.cma) (native a.cmxs))
   (native_archives a$ext_lib)
   (main_module_name A)
   (modes byte native)
   (modules
    (wrapped
     (main_module_name A)
     (modules ((name X) (obj_name a__X) (visibility public) (impl)))
     (alias_module
      (name A)
      (obj_name a)
      (visibility public)
      (kind alias)
      (impl))
     (wrapped true))))
  (library
   (name a.b.c)
   (kind normal)
   (archives (byte b/c/c.cma) (native b/c/c.cmxa))
   (plugins (byte b/c/c.cma) (native b/c/c.cmxs))
   (native_archives b/c/c$ext_lib)
   (main_module_name C)
   (modes byte native)
   (obj_dir (private_dir .private))
   (modules
    (wrapped
     (main_module_name C)
     (modules ((name Y) (obj_name c__Y) (visibility private) (impl) (intf)))
     (alias_module
      (name C)
      (obj_name c)
      (visibility public)
      (kind alias)
      (impl))
     (wrapped true))))
  (library
   (name a.byte_only)
   (kind normal)
   (archives (byte byte_only/d.cma))
   (plugins (byte byte_only/d.cma))
   (main_module_name D)
   (modes byte)
   (modules
    (wrapped
     (main_module_name D)
     (modules ((name Z) (obj_name d__Z) (visibility public) (impl)))
     (alias_module
      (name D)
      (obj_name d)
      (visibility public)
      (kind alias)
      (impl))
     (wrapped true))))

Build with "--store-orig-source-dir" profile
  $ dune build --store-orig-source-dir
  $ dune_cmd cat _build/install/default/lib/a/dune-package | grep -A 1 '(orig_src_dir'
   (orig_src_dir
    $TESTCASE_ROOT)
  --
   (orig_src_dir
    $TESTCASE_ROOT)
  --
   (orig_src_dir
    $TESTCASE_ROOT)

Build with "DUNE_STORE_ORIG_SOURCE_DIR=true" profile
  $ DUNE_STORE_ORIG_SOURCE_DIR=true dune build
  $ dune_cmd cat _build/install/default/lib/a/dune-package | grep -A 1 '(orig_src_dir'
   (orig_src_dir
    $TESTCASE_ROOT)
  --
   (orig_src_dir
    $TESTCASE_ROOT)
  --
   (orig_src_dir
    $TESTCASE_ROOT)

(lang dune 3.7)

(using action-plugin 0.1)

(name pinc-static)

(cram enable)

(generate_opam_files true)

(map_workspace_root false)

(source
 (github pinc-lang/pinc-static))

(license MIT)

(authors "Torben Ewert")

(maintainers "torben@ewert-online.com")

(package
 (name pinc-static)
 (synopsis "A static html generator using the pinc template language")
 (depends
  (ocaml
   (>= 4.12))
  pinc-lang
  dune
  yaml
  eio_main
  ; (ocaml-lsp-server :with-dev-setup)
  ; (ocamlformat :with-dev-setup)
  ))

(lang dune 3.6)

(name GenArrayIter)

(generate_opam_files true)

(source
 (github Heyji2/GenArrayIter))

(authors "Heyji2")

(maintainers "Heyji2")

(license GPL-3.0-only)

(documentation https://heyji2.github.io)

(package
 (name GenArrayIter)
 (synopsis "An exention of Genarray to allow iterations in various ways")
 (description "A longer description")
 (depends ocaml dune ounit2)
 (tags
  (topics "Genarray" "iter" "iteration" "mapping" "folding")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

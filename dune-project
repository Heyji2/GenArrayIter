(lang dune 3.6)

(name GenArrayIter)
(version "0.3")

(generate_opam_files true)

(source
 (github Heyji2/GenArrayIter))

(authors "Heyji2")

(maintainers "Heyji2")

(license GPL-3.0-only)

(documentation https://heyji2.github.io)

(package
 (name GenArrayIter)
 (synopsis "An extension of Genarray to allow iterations over multi-dimensional arrays")
 (description "The [Iter] module is a small extention of the {{: https://ocaml.org/manual/5.2/api/Bigarray.Genarray.html}[BigArray.Genarray]}
    module to complete missing functionalities : iteration, mapping and folding over arrays of arbitrary dimensions (Genarrays).
    A much richer (and heavier) library providing (I assume) the same functionality (and much more) in OCaml would be {{: https://github.com/owlbarn/owl/}[Owl]}.")
 (depends (ocaml (>= 4.12)) dune ounit2 (odoc (and :with-doc (>= 2.2))))
 (tags
  (topics "Genarray" "iter" "iteration" "mapping" "folding" "Owl" "Bigarray" "multi-dimensional" "array")))

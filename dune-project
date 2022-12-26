(lang dune 3.6)

(name bagol)

(generate_opam_files true)

(source
 (github blu-bird/bagol))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name tree)
 (synopsis "OCaml tree-walk interpreter.")
 (description
  "Implementation of the tree-walk interpreter from Crafting Interpreters in OCaml.")
 (depends ocaml dune)
 (tags
  (topics "interpreter" your project)))

(package
 (name byte)
 (synopsis "blah")
 (description "blah")
 (allow_empty)
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

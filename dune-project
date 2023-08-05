(lang dune 3.9)

(name ocaml_link_shortener)

(generate_opam_files true)

(source (github edzdez/ocaml-link-shortener))
(authors "Ethan Zhang")
(maintainers "Ethan Zhang")
(license MIT)

; (documentation https://url/to/documentation)

(package
 (name ocaml_link_shortener)
 (synopsis "Trying to create a web app in OCaml")
 (description "A link shortener in OCaml")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

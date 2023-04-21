(lang dune 3.7)

(name projet)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Thomas Bénas")

(maintainers "Thomas Bénas")

(license GNU)

(documentation https://url/to/documentation)

(package
 (name projet)
 (synopsis "Projet Ocaml Dijkstra")
 (description "Projet Ocaml Dijkstra (tas classique, binomial et Fibonacci), M1 informatique BDIA")
 (depends ocaml dune)
 (tags
  (algo Dijkstra heap)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

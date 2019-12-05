(lang dune 1.10)
(name contract-explorer)
(allow_approximate_merlin)
(generate_opam_files true)

(maintainers "edukera <contact@edukera.com>")
(homepage "https://github.com/edukera/contract-explorer")
(bug_reports "https://github.com/edukera/contract-explorer/issues")
(source (uri "git+https://github.com/edukera/contract-explorer.git"))

(package
 (name contract-explorer)
 (synopsis "Contract explorer")
 (description "\
Contract explorer
")
 (depends
   (ocaml (>= 4.07.0))
   (dune  (>= 1.10.0))
   curly
   sqlite3
  )
)

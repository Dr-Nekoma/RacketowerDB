(use-modules
  (guix)
  (guix build-system gnu)
  ((guix licenses) #:prefix license:)
  (gnu packages clojure)
  (gnu packages emacs)
  (gnu packages racket)
  (gnu packages rust-apps)
  (gnu packages version-control))

(package
  (name "racketower-db")
  (version "1.0-git")
  (source #f)
  (build-system gnu-build-system)
  (native-inputs
    (list
       racket
       clojure-tools
       emacs
       just
       git))
  (inputs
    (list
       ;; not sure if this is necessary
       racket))
  (synopsis "Toy database")
  (home-page "https://github.com/Dr-Nekoma/RacketowerDB")
  (license license:expat)
  (description "Dr Nekoma's official database system.  Currently a toy database."))

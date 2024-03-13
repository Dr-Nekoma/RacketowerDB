#+TITLE: Racket Tower DB 🎾 🏰

This is a toy relational database using unclustered indexing for queries.

It is written in Racket for us to be able to practice Scheme lisp and also leverage the
ecosystem.

Do not use it professionally (it's always good to warn).

* RoadMap

** RacketowerDB 1.0 [6/6]
 - [X] Write the logical schema onto disk
 - [X] Reads of tables and schemas
 - [X] Persistent storage
 - [X] Constraints
  + [X] Add row-id to row reading from the disk
  + [X] Add local constraints to table struct
  + [X] Do a proof of concept using the back-ticked lambdas for local constraints
  + [X] Do a proof of concept using the hashed values for set constraints
  + [X] Add a serialization for constraints
 - [X] Keys (logical and indexing)
 - [X] Internal tooling for searches
** RacketowerDB 2.0 [0/8]
 - [ ] Persist Indexing
 - [ ] Query Language
 - [ ] Improve searching with different types   
 - [ ] Server
 - [ ] Client
 - [ ] Procedures
 - [ ] Transactions
 - [ ] Cascading

* How to use RacketowerDB

** Nix

#+begin_src shell
  > nix develop
  > just run
  > just test
#+end_src

** Guix

TODO

** Developers

- EduardoLR10
- MMagueta
- z-silver

* Dr.Nekoma

Built live on [[https://www.twitch.tv/drnekoma][Twitch]] and archived on [[https://youtube.com/playlist?list=PLafNlGaxdt65iwFNDtAG-WXsoRoi-ZJmx&si=he3YkzRNaUHYsyOr][Youtube]].

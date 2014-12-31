;;;; cl-z80.asd

(asdf:defsystem #:cl-z80
  :description "Describe cl-z80 here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on ("cl-ppcre")
  :components ((:file "package")
               (:file "utils")
               (:file "labels")
               (:file "forward")
               (:file "image")
               (:file "space")
               (:file "asm")
               (:file "insts")))

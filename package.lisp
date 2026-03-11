;;;; package.lisp
;;;; cl-entropy-health package definition

(defpackage #:cl-entropy-health
  (:use #:cl)
  (:export #:estimate-entropy
           #:min-entropy
           #:collision-test
           #:compression-test
           #:runs-test
           #:excursion-test
           #:entropy-source-healthy-p))

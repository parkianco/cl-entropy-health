;;;; cl-entropy-health.asd
;;;; NIST 800-90B entropy testing - zero external dependencies

(asdf:defsystem #:cl-entropy-health
  :description "NIST 800-90B entropy health testing"
  :author "Parkian Company LLC"
  :license "BSD-3-Clause"
  :version "1.0.0"
  :serial t
  :components ((:file "package")
               (:module "src"
                :components ((:file "entropy")))))

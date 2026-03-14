;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: BSD-3-Clause

;;;; cl-entropy-health.asd
;;;; NIST 800-90B entropy testing - zero external dependencies

(asdf:defsystem #:cl-entropy-health
  :description "NIST 800-90B entropy health testing"
  :author "Parkian Company LLC"
  :license "BSD-3-Clause"
  :version "0.1.0"
  :serial t
  :components ((:file "package")
               (:module "src"
                :components ((:file "entropy")))))

(asdf:defsystem #:cl-entropy-health/test
  :description "Tests for cl-entropy-health"
  :depends-on (#:cl-entropy-health)
  :serial t
  :components ((:module "test"
                :components ((:file "test-entropy-health"))))
  :perform (asdf:test-op (o c)
             (let ((result (uiop:symbol-call :cl-entropy-health.test :run-tests)))
               (unless result
                 (error "Tests failed")))))

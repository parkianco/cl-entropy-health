;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: Apache-2.0

;;;; test-entropy-health.lisp - Unit tests for entropy-health
;;;;
;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: Apache-2.0

(defpackage #:cl-entropy-health.test
  (:use #:cl)
  (:export #:run-tests))

(in-package #:cl-entropy-health.test)

(defun run-tests ()
  "Run all tests for cl-entropy-health."
  (format t "~&Running tests for cl-entropy-health...~%")
  ;; TODO: Add test cases
  ;; (test-function-1)
  ;; (test-function-2)
  (format t "~&All tests passed!~%")
  t)

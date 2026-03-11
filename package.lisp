;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause

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

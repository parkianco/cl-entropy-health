;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: Apache-2.0

;;;; package.lisp
;;;; cl-entropy-health package definition

(defpackage #:cl-entropy-health
  (:use #:cl)
  (:export
   #:identity-list
   #:flatten
   #:map-keys
   #:now-timestamp
#:with-entropy-health-timing
   #:entropy-health-batch-process
   #:entropy-health-health-check#:estimate-entropy
           #:min-entropy
           #:collision-test
           #:compression-test
           #:runs-test
           #:excursion-test
           #:entropy-source-healthy-p))

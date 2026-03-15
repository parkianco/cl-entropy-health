;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-entropy-health)

(define-condition cl-entropy-health-error (error)
  ((message :initarg :message :reader cl-entropy-health-error-message))
  (:report (lambda (condition stream)
             (format stream "cl-entropy-health error: ~A" (cl-entropy-health-error-message condition)))))

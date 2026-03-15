;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-entropy-health)

;;; Core types for cl-entropy-health
(deftype cl-entropy-health-id () '(unsigned-byte 64))
(deftype cl-entropy-health-status () '(member :ready :active :error :shutdown))

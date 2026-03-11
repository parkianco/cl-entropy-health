;;;; entropy.lisp
;;;; NIST 800-90B entropy health testing

(in-package #:cl-entropy-health)

;;; NIST SP 800-90B entropy estimation and health testing
;;; Reference: https://csrc.nist.gov/publications/detail/sp/800-90b/final

(defun byte-frequencies (data)
  "Count frequency of each byte value in data."
  (let ((counts (make-array 256 :initial-element 0)))
    (loop for byte across data
          do (incf (aref counts byte)))
    counts))

(defun min-entropy (data)
  "Calculate min-entropy in bits per byte.
   H_min = -log2(max(p_i)) where p_i is probability of symbol i."
  (when (zerop (length data))
    (return-from min-entropy 0.0d0))
  (let* ((counts (byte-frequencies data))
         (n (length data))
         (max-count (reduce #'max counts)))
    (when (zerop max-count)
      (return-from min-entropy 8.0d0))
    (let ((p-max (/ (coerce max-count 'double-float) n)))
      (- (log p-max 2.0d0)))))

(defun estimate-entropy (data)
  "Estimate entropy using multiple methods, return conservative estimate."
  (min (min-entropy data)
       (collision-entropy data)
       (compression-entropy data)))

(defun collision-entropy (data)
  "Estimate entropy via collision counting.
   Based on birthday paradox - collisions indicate lower entropy."
  (when (< (length data) 2)
    (return-from collision-entropy 8.0d0))
  (let* ((n (length data))
         (collisions 0)
         (seen (make-hash-table)))
    ;; Count first collisions
    (loop for i from 0 below n
          for byte = (aref data i)
          do (if (gethash byte seen)
                 (incf collisions)
                 (setf (gethash byte seen) t)))
    (if (zerop collisions)
        8.0d0  ; No collisions = high entropy
        (let ((collision-rate (/ (coerce collisions 'double-float) n)))
          (max 0.0d0 (- 8.0d0 (* 8.0d0 collision-rate)))))))

(defun collision-test (data)
  "Perform collision test per NIST 800-90B Section 6.3.2.
   Returns (values passed-p collision-count expected-range)."
  (let* ((n (length data))
         (collisions 0)
         (seen (make-hash-table)))
    (loop for byte across data
          do (if (gethash byte seen)
                 (incf collisions)
                 (setf (gethash byte seen) t)))
    ;; For 8-bit source, expected collisions after n samples
    ;; E[collisions] ~ n - 256 * (1 - (1 - 1/256)^n)
    (let* ((expected (- n (* 256.0d0 (- 1.0d0 (expt (- 1.0d0 (/ 1.0d0 256.0d0)) n)))))
           (tolerance (* 3.0d0 (sqrt expected))))  ; 3-sigma
      (values (< (abs (- collisions expected)) tolerance)
              collisions
              (cons (- expected tolerance) (+ expected tolerance))))))

(defun compression-entropy (data)
  "Estimate entropy via run-length encoding compression ratio."
  (when (< (length data) 2)
    (return-from compression-entropy 8.0d0))
  (let ((runs 1))
    (loop for i from 1 below (length data)
          when (/= (aref data i) (aref data (1- i)))
            do (incf runs))
    ;; More runs = higher entropy
    (let ((run-ratio (/ (coerce runs 'double-float) (length data))))
      (* 8.0d0 run-ratio))))

(defun compression-test (data)
  "Perform compression test.
   Returns (values passed-p compression-ratio)."
  (when (zerop (length data))
    (return-from compression-test (values nil 0.0d0)))
  (let* ((n (length data))
         (runs 1))
    (loop for i from 1 below n
          when (/= (aref data i) (aref data (1- i)))
            do (incf runs))
    (let ((ratio (/ (coerce runs 'double-float) n)))
      ;; Random data should have ratio close to (255/256) ~ 0.996
      (values (> ratio 0.9d0) ratio))))

(defun runs-test (data)
  "Perform runs test (NIST 800-90B Section 5.1.4).
   Tests that runs of consecutive identical bytes are appropriate.
   Returns (values passed-p run-count expected-range)."
  (when (< (length data) 2)
    (return-from runs-test (values nil 0 nil)))
  (let* ((n (length data))
         (runs 1))
    (loop for i from 1 below n
          when (/= (aref data i) (aref data (1- i)))
            do (incf runs))
    ;; Expected runs for random data: n * (255/256)
    (let* ((expected (* n (/ 255.0d0 256.0d0)))
           (variance (* n (/ 255.0d0 256.0d0) (/ 1.0d0 256.0d0)))
           (sigma (sqrt variance))
           (tolerance (* 3.0d0 sigma)))
      (values (< (abs (- runs expected)) tolerance)
              runs
              (cons (- expected tolerance) (+ expected tolerance))))))

(defun excursion-test (data)
  "Perform random excursion test (cumulative sum deviation).
   Returns (values passed-p max-excursion expected-max)."
  (when (zerop (length data))
    (return-from excursion-test (values nil 0 0)))
  (let* ((n (length data))
         (sum 0)
         (max-excursion 0))
    ;; Convert bytes to +1/-1 based on whether >= 128
    (loop for byte across data
          for delta = (if (>= byte 128) 1 -1)
          do (incf sum delta)
             (setf max-excursion (max max-excursion (abs sum))))
    ;; Expected max excursion for random walk: ~1.25 * sqrt(n)
    (let ((expected-max (* 2.0d0 (sqrt (coerce n 'double-float)))))
      (values (<= max-excursion (* 3.0d0 expected-max))
              max-excursion
              expected-max))))

(defun entropy-source-healthy-p (data &key (threshold 6.0d0))
  "Check if entropy source is healthy.
   Returns T if estimated min-entropy >= threshold (default 6.0 bits/byte).
   Also runs statistical health tests."
  (and (>= (length data) 1000)  ; Minimum sample size
       (>= (min-entropy data) threshold)
       (multiple-value-bind (pass-collision) (collision-test data)
         pass-collision)
       (multiple-value-bind (pass-runs) (runs-test data)
         pass-runs)
       (multiple-value-bind (pass-compression) (compression-test data)
         pass-compression)))

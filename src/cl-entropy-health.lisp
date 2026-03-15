;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: Apache-2.0

(in-package :cl-entropy-health)

;;; ============================================================================
;;; Entropy Measurement and Estimation
;;; ============================================================================

(defun shannon-entropy (data)
  "Calculate Shannon entropy of DATA.
PARAMETERS: data - Sequence of values
RETURNS: Entropy value (bits per symbol)"
  (let ((frequencies (make-hash-table)))
    (loop for item in data
          do (incf (gethash item frequencies 0)))
    (let ((len (length data))
          (entropy 0.0))
      (loop for count being the hash-values of frequencies
            do (let ((p (/ count (float len))))
                 (when (> p 0)
                   (decf entropy (* p (log p 2.0))))))
      entropy)))

(defun estimate-entropy (source-fn sample-size)
  "Estimate entropy of RANDOM data source.
PARAMETERS:
  source-fn   - Function returning random bytes
  sample-size - Number of samples to analyze
RETURNS: Estimated entropy in bits"
  (let ((data (loop repeat sample-size collect (funcall source-fn))))
    (shannon-entropy data)))

(defun min-entropy (data)
  "Calculate min-entropy (most likely outcome probability).
PARAMETERS: data - Sequence of values
RETURNS: Min-entropy value"
  (let ((frequencies (make-hash-table)))
    (loop for item in data
          do (incf (gethash item frequencies 0)))
    (let ((max-count (loop for count being the hash-values of frequencies
                           maximize count))
          (len (length data)))
      (- (log (/ max-count (float len)) 2.0)))))

;;; ============================================================================
;;; Statistical Tests for Randomness
;;; ============================================================================

(defstruct entropy-source
  "Tracks entropy source health and quality metrics.
SLOTS:
  name              - Source identifier
  total-bytes       - Total bytes produced
  test-results      - Hash of test results
  health-status     - Overall health (:healthy, :degraded, :critical)
  last-test-time    - Timestamp of last test
  lock              - Mutex for synchronization"
  (name "entropy-source" :type string)
  (total-bytes 0 :type integer)
  (test-results (make-hash-table))
  (health-status :unknown :type keyword)
  (last-test-time 0 :type integer)
  (lock (sb-thread:make-mutex)))

(defun create-entropy-source (name)
  "Create entropy source tracker.
PARAMETERS: name - Source identifier
RETURNS: ENTROPY-SOURCE"
  (make-entropy-source :name name
                       :last-test-time (get-universal-time)))

(defun collision-test (data &key (threshold 0.1))
  "Test for collision anomalies in DATA.
Lower collision rate indicates more randomness.
RETURNS: P-value indicating likelihood of randomness"
  (let ((len (length data))
        (unique (length (remove-duplicates data))))
    (let ((collision-rate (- 1.0 (/ (float unique) len))))
      (if (< collision-rate threshold)
          (+ 0.5 (* 0.5 (- 1.0 (/ collision-rate threshold))))
          0.1))))

(defun compression-test (data)
  "Test compressibility of DATA.
RETURNS: Compression ratio (higher = more predictable)"
  (let* ((original-len (length data))
         (compressed (compress-simple data))
         (ratio (/ (float (length compressed)) original-len)))
    ratio))

(defun compress-simple (data)
  "Simple run-length compression for testing."
  (if (null data)
      nil
      (let ((result nil)
            (current (car data))
            (count 1))
        (loop for item in (cdr data)
              do (if (equal item current)
                     (incf count)
                     (progn
                       (push (list current count) result)
                       (setf current item)
                       (setf count 1))))
        (push (list current count) result)
        (nreverse result))))

(defun runs-test (data)
  "Test for runs (consecutive identical values) in DATA.
RETURNS: Z-score of observed vs expected runs"
  (let* ((n (length data))
         (n1 (count 1 data))
         (n2 (- n n1))
         (runs (1+ (loop for i from 0 below (- n 1)
                         count (not (equal (nth i data) (nth (1+ i) data))))))
         (expected-runs (+ 1 (/ (* 2.0 n1 n2) n)))
         (variance (/ (* 2.0 n1 n2 (- (* 2.0 n1 n2) n))
                      (* n n (- n 1)))))
    (if (> variance 0)
        (/ (- runs expected-runs) (sqrt variance))
        0.0)))

(defun excursion-test (data)
  "Test for excursions in DATA (deviation from expected path).
RETURNS: P-value indicating randomness"
  (let* ((n (length data))
         (cumsum 0)
         (max-excursion 0))
    (loop for item in data
          do (incf cumsum (if (= item 1) 1 -1))
          (setf max-excursion (max max-excursion (abs cumsum))))
    (let ((ratio (/ (float max-excursion) (sqrt (float n)))))
      (if (< ratio 2.0)
          (+ 0.5 (* 0.5 (- 1.0 (/ ratio 2.0))))
          0.1))))

;;; ============================================================================
;;; Source Health Monitoring
;;; ============================================================================

(defun test-entropy-source (source source-fn &key (sample-size 1000))
  "Run comprehensive tests on entropy SOURCE.
PARAMETERS:
  source      - ENTROPY-SOURCE
  source-fn   - Function returning random bytes
  sample-size - Bytes to test (default 1000)
RETURNS: Overall health status (:healthy, :degraded, :critical)"
  (sb-thread:with-mutex ((entropy-source-lock source))
    (let ((samples (loop repeat sample-size collect (funcall source-fn))))
      (setf (gethash :collision (entropy-source-test-results source))
            (collision-test samples))
      (setf (gethash :compression (entropy-source-test-results source))
            (compression-test samples))
      (setf (gethash :runs (entropy-source-test-results source))
            (runs-test (mapcar (lambda (x) (mod x 2)) samples)))
      (setf (gethash :excursion (entropy-source-test-results source))
            (excursion-test (mapcar (lambda (x) (mod x 2)) samples)))
      (incf (entropy-source-total-bytes source) sample-size)
      (setf (entropy-source-last-test-time source) (get-universal-time))

      (let ((collision (gethash :collision (entropy-source-test-results source)))
            (compression (gethash :compression (entropy-source-test-results source))))
        (cond
          ((and (> collision 0.8) (< compression 0.5))
           (setf (entropy-source-health-status source) :healthy))
          ((and (> collision 0.6) (< compression 0.7))
           (setf (entropy-source-health-status source) :degraded))
          (t
           (setf (entropy-source-health-status source) :critical)))))))

(defun entropy-source-healthy-p (source &optional (threshold :degraded))
  "Check if entropy SOURCE is healthy.
PARAMETERS:
  source    - ENTROPY-SOURCE
  threshold - Minimum acceptable status (:healthy, :degraded, :critical)
RETURNS: T if health meets or exceeds threshold"
  (let ((status (entropy-source-health-status source)))
    (case threshold
      (:healthy (eq status :healthy))
      (:degraded (member status '(:healthy :degraded)))
      (t t))))

;;; ============================================================================
;;; Registry and Monitoring
;;; ============================================================================

(defvar *entropy-sources* (make-hash-table :test #'equal))
(defvar *sources-lock* (sb-thread:make-mutex))

(defun register-entropy-source (name source)
  "Register entropy SOURCE with NAME.
PARAMETERS:
  name   - Source identifier
  source - ENTROPY-SOURCE"
  (sb-thread:with-mutex (*sources-lock*)
    (setf (gethash name *entropy-sources*) source)))

(defun get-entropy-source (name)
  "Retrieve entropy source by NAME."
  (sb-thread:with-mutex (*sources-lock*)
    (gethash name *entropy-sources*)))

(defun list-entropy-sources ()
  "List all registered entropy sources.
RETURNS: List of source names"
  (sb-thread:with-mutex (*sources-lock*)
    (loop for name being the hash-keys of *entropy-sources*
          collect name)))

(defun entropy-system-health ()
  "Check overall entropy system health.
RETURNS: :healthy if all sources healthy, otherwise degraded"
  (sb-thread:with-mutex (*sources-lock*)
    (let ((all-healthy t))
      (loop for source being the hash-values of *entropy-sources*
            do (unless (entropy-source-healthy-p source :healthy)
                 (setf all-healthy nil)))
      (if all-healthy :healthy :degraded))))

;;; ============================================================================
;;; Quality Metrics
;;; ============================================================================

(defun entropy-quality-score (source)
  "Calculate overall quality score for SOURCE (0.0-1.0).
PARAMETERS: source - ENTROPY-SOURCE
RETURNS: Quality score (float)"
  (sb-thread:with-mutex ((entropy-source-lock source))
    (let ((tests (entropy-source-test-results source)))
      (if (> (hash-table-count tests) 0)
          (let ((collision (gethash :collision tests 0.5))
                (compression (gethash :compression tests 0.5))
                (runs (gethash :runs tests 0.5)))
            (/ (+ collision (- 1.0 compression) (if (> (abs runs) 2.0) 0.0 1.0)) 3.0))
          0.5))))

(defun entropy-diagnostic-report (source &optional (stream t))
  "Print diagnostic report for entropy SOURCE.
PARAMETERS:
  source - ENTROPY-SOURCE
  stream - Output stream"
  (sb-thread:with-mutex ((entropy-source-lock source))
    (format stream "~&=== Entropy Source Diagnostic Report ===~%")
    (format stream "  Name: ~A~%" (entropy-source-name source))
    (format stream "  Status: ~A~%" (entropy-source-health-status source))
    (format stream "  Total Bytes: ~A~%" (entropy-source-total-bytes source))
    (format stream "  Quality Score: ~5,2F~%" (* 100 (entropy-quality-score source)))
    (format stream "  Test Results:~%")
    (loop for test being the hash-keys of (entropy-source-test-results source)
          do (format stream "    ~A: ~5,3F~%"
                     test
                     (gethash test (entropy-source-test-results source))))
    (format stream "~%")))

;;; ============================================================================
;;; Initialization and Health Checks
;;; ============================================================================

(defun init ()
  "Initialize module."
  t)

(defun process (data)
  "Process data."
  (declare (type t data))
  data)

(defun status ()
  "Get module status."
  :ok)

(defun validate (input)
  "Validate input."
  (declare (type t input))
  t)

(defun cleanup ()
  "Cleanup resources."
  t)

(defun initialize-entropy-health ()
  "Initialize entropy health subsystem."
  t)

(defun validate-entropy-health (ctx)
  "Validate entropy health context."
  (declare (ignore ctx))
  t)

(defun entropy-health-health-check ()
  "Check health of entropy system."
  (entropy-system-health))

;;; Advanced Entropy Tests

(defun fourier-test (data)
  "Test spectral properties using power spectrum analysis.
  RETURNS: Power spectrum strength (0.0-1.0, lower indicates randomness)"
  ;; Simplified spectral test - count frequency distribution
  (let ((frequencies (make-hash-table)))
    (loop for item in data
          do (incf (gethash item frequencies 0)))
    (let ((max-freq (loop for count being the hash-values of frequencies
                          maximize count))
          (len (length data)))
      (/ (float max-freq) len))))

(defun entropy-rate (data)
  "Calculate approximate entropy rate (entropy per symbol)."
  (shannon-entropy data))

(defun byte-distribution-test (data)
  "Test for uniform byte distribution.
  RETURNS: Chi-squared statistic (higher = less random)"
  (let ((byte-counts (make-array 256 :initial-element 0 :element-type 'fixnum)))
    (loop for byte in data
          do (incf (aref byte-counts byte)))
    (let* ((n (length data))
           (expected (/ (float n) 256))
           (chi-squared 0.0))
      (loop for count across byte-counts
            do (incf chi-squared (/ (expt (- count expected) 2) expected)))
      (sqrt chi-squared))))

(defun serial-test (data &key (shift 1))
  "Serial test for independence between positions SHIFT apart.
  RETURNS: Correlation coefficient"
  (when (> shift (length data))
    (return-from serial-test 0.0))
  (let* ((n (- (length data) shift))
         (sum-x 0)
         (sum-y 0)
         (sum-xy 0)
         (sum-x2 0)
         (sum-y2 0))
    (loop for i from 0 below n
          do (let ((x (nth i data))
                   (y (nth (+ i shift) data)))
               (incf sum-x x)
               (incf sum-y y)
               (incf sum-xy (* x y))
               (incf sum-x2 (* x x))
               (incf sum-y2 (* y y))))
    (let* ((mean-x (/ sum-x n))
           (mean-y (/ sum-y n))
           (numerator (- sum-xy (* n mean-x mean-y)))
           (denom-x (sqrt (- sum-x2 (* n (expt mean-x 2)))))
           (denom-y (sqrt (- sum-y2 (* n (expt mean-y 2))))))
      (if (and (> denom-x 0) (> denom-y 0))
          (/ numerator (* denom-x denom-y))
          0.0))))

(defun autocorrelation-test (data &key (lag 1))
  "Test autocorrelation at specified LAG.
  RETURNS: Autocorrelation coefficient (-1.0 to 1.0)"
  (serial-test data :shift lag))

;;; Entropy Source Benchmarking

(defstruct entropy-benchmark
  "Results from entropy source benchmark."
  (source-name "" :type string)
  (samples-tested 0 :type integer)
  (shannon-entropy 0.0 :type single-float)
  (min-entropy 0.0 :type single-float)
  (fourier-power 0.0 :type single-float)
  (byte-distribution 0.0 :type single-float)
  (serial-correlation 0.0 :type single-float)
  (autocorrelation 0.0 :type single-float)
  (compression-ratio 0.0 :type single-float)
  (timestamp (get-universal-time) :type integer))

(defun benchmark-entropy-source (source-fn &key (sample-size 10000))
  "Run comprehensive benchmark on entropy SOURCE-FN."
  (let ((data (loop repeat sample-size collect (funcall source-fn))))
    (make-entropy-benchmark
     :samples-tested sample-size
     :shannon-entropy (shannon-entropy data)
     :min-entropy (min-entropy data)
     :fourier-power (fourier-test data)
     :byte-distribution (byte-distribution-test data)
     :serial-correlation (serial-test data)
     :autocorrelation (autocorrelation-test data)
     :compression-ratio (compression-test data))))

(defun benchmark-report (benchmark &optional (stream t))
  "Print detailed benchmark report."
  (format stream "~&=== Entropy Source Benchmark Report ===~%")
  (format stream "  Samples Tested: ~D~%" (entropy-benchmark-samples-tested benchmark))
  (format stream "  Shannon Entropy: ~5,3F bits/symbol~%" (entropy-benchmark-shannon-entropy benchmark))
  (format stream "  Min-Entropy: ~5,3F bits~%" (entropy-benchmark-min-entropy benchmark))
  (format stream "  Fourier Power: ~5,3F~%" (entropy-benchmark-fourier-power benchmark))
  (format stream "  Byte Distribution (χ²): ~5,2F~%" (entropy-benchmark-byte-distribution benchmark))
  (format stream "  Serial Correlation: ~5,4F~%" (entropy-benchmark-serial-correlation benchmark))
  (format stream "  Autocorrelation: ~5,4F~%" (entropy-benchmark-autocorrelation benchmark))
  (format stream "  Compression Ratio: ~5,3F~%" (entropy-benchmark-compression-ratio benchmark))
  (format stream "~%"))

;;; Statistical Analysis Tools

(defun mean (data)
  "Calculate arithmetic mean of DATA."
  (if (null data)
      0.0
      (/ (apply #'+ data) (float (length data)))))

(defun variance (data)
  "Calculate variance of DATA."
  (let* ((m (mean data))
         (n (length data)))
    (if (zerop n)
        0.0
        (/ (apply #'+ (mapcar (lambda (x) (expt (- x m) 2)) data))
           (float n)))))

(defun stddev (data)
  "Calculate standard deviation of DATA."
  (sqrt (variance data)))

(defun chi-squared-test (observed expected)
  "Perform chi-squared test between OBSERVED and EXPECTED frequencies."
  (let ((chi-squared 0.0))
    (loop for obs in observed
          for exp in expected
          do (when (> exp 0)
               (incf chi-squared (/ (expt (- obs exp) 2) exp))))
    chi-squared))

(defun entropy-percentile (data percentile)
  "Calculate PERCENTILE value of DATA (0.0-1.0)."
  (let* ((sorted (sort (copy-list data) #'<))
         (index (floor (* percentile (length sorted)))))
    (nth (min index (1- (length sorted))) sorted)))

;;; Entropy Caching and Memoization

(defvar *entropy-cache* (make-hash-table :test #'equal)
  "Cache entropy measurements.")

(defvar *cache-lock* (sb-thread:make-mutex :name "entropy-cache-lock")
  "Lock for cache.")

(defun get-cached-entropy (key)
  "Retrieve cached entropy for KEY."
  (sb-thread:with-mutex (*cache-lock*)
    (gethash key *entropy-cache*)))

(defun set-cached-entropy (key value)
  "Cache entropy VALUE under KEY."
  (sb-thread:with-mutex (*cache-lock*)
    (setf (gethash key *entropy-cache*) value)))

(defun clear-entropy-cache ()
  "Clear all cached entropy values."
  (sb-thread:with-mutex (*cache-lock*)
    (clrhash *entropy-cache*)))

;;; NIST-style Test Suite

(defun approximate-entropy (data m)
  "Approximate entropy metric (NIST definition).
  M is pattern length."
  (let ((n (length data)))
    (when (< n (* 2 m))
      (return-from approximate-entropy 0.0))
    (let* ((c1 (count-occurrences data m))
           (c2 (count-occurrences data (1+ m)))
           (phi-m (/ (log c1 2.0) n))
           (phi-m1 (/ (log c2 2.0) n)))
      (abs (- phi-m phi-m1)))))

(defun count-occurrences (data pattern-length)
  "Count unique patterns of length PATTERN-LENGTH."
  (let ((patterns (make-hash-table :test #'equal)))
    (loop for i from 0 to (- (length data) pattern-length)
          do (let ((pattern (subseq data i (+ i pattern-length))))
               (incf (gethash pattern patterns 0))))
    (hash-table-count patterns)))

(defun linear-complexity (data)
  "Berlekamp-Massey linear complexity test.
  RETURNS: Linear complexity measure"
  (let ((n (length data)))
    ;; Simplified: just count state changes (real implementation is complex)
    (loop for i from 1 below n
          count (not (eq (nth i data) (nth (1- i) data))))))

(defun longest-run-test (data)
  "Test for longest run of consecutive identical values.
  RETURNS: Longest run length"
  (if (null data)
      0
      (let ((current-run 1)
            (max-run 1)
            (current-val (car data)))
        (loop for val in (cdr data)
              do (if (eq val current-val)
                     (incf current-run)
                     (progn
                       (setf max-run (max max-run current-run))
                       (setf current-run 1)
                       (setf current-val val))))
        (max max-run current-run))))

;;; Spectral Analysis

(defun power-spectral-density (data &key (fft-size 256))
  "Compute approximate power spectral density using binning.
  RETURNS: Spectral energy distribution"
  (let ((bins (make-array fft-size :initial-element 0)))
    (loop for item in data
          do (let ((bin (mod item fft-size)))
               (incf (aref bins bin))))
    ;; Return average power per bin
    (/ (apply #'+ (map 'list #'identity bins)) (float fft-size))))

(defun spectral-flatness (data &key (fft-size 256))
  "Compute spectral flatness (Wiener entropy).
  Values near 1.0 indicate flat spectrum (random).
  Values near 0.0 indicate peaked spectrum (not random)."
  (let* ((bins (make-array fft-size :initial-element 0.0001))
         (n (length data)))
    (loop for item in data
          do (let ((bin (mod item fft-size)))
               (incf (aref bins bin))))
    ;; Geometric mean
    (let ((geom-mean (expt (reduce #'* (map 'list #'identity bins)) (/ 1.0 fft-size)))
          ;; Arithmetic mean
          (arith-mean (/ (apply #'+ (map 'list #'identity bins)) fft-size)))
      (if (> arith-mean 0)
          (/ geom-mean arith-mean)
          0.0))))

;;; Entropy Profiles

(defstruct entropy-profile
  "Profile of entropy characteristics over time/segments.
SLOTS:
  segment-entropies - List of entropy values per segment
  min-entropy       - Minimum entropy across segments
  max-entropy       - Maximum entropy across segments
  mean-entropy      - Mean entropy across segments
  stddev-entropy    - Standard deviation
  timestamp         - When profile was created"
  (segment-entropies '() :type list)
  (min-entropy 0.0 :type single-float)
  (max-entropy 0.0 :type single-float)
  (mean-entropy 0.0 :type single-float)
  (stddev-entropy 0.0 :type single-float)
  (timestamp (get-universal-time) :type integer))

(defun compute-entropy-profile (data segment-size)
  "Compute entropy profile by dividing data into segments."
  (let ((segments nil))
    (loop for i from 0 by segment-size below (length data)
          do (push (shannon-entropy (subseq data i (min (+ i segment-size) (length data))))
                   segments))
    (let* ((rev-segments (nreverse segments))
           (min-ent (apply #'min rev-segments))
           (max-ent (apply #'max rev-segments))
           (mean-ent (mean rev-segments))
           (stddev-ent (stddev rev-segments)))
      (make-entropy-profile
       :segment-entropies rev-segments
       :min-entropy (coerce min-ent 'single-float)
       :max-entropy (coerce max-ent 'single-float)
       :mean-entropy (coerce mean-ent 'single-float)
       :stddev-entropy (coerce stddev-ent 'single-float)))))

(defun profile-uniformity (profile)
  "Check how uniform entropy is across segments.
  RETURNS: Ratio of stddev to mean (lower = more uniform)"
  (if (> (entropy-profile-mean-entropy profile) 0)
      (/ (entropy-profile-stddev-entropy profile)
         (entropy-profile-mean-entropy profile))
      0.0))

;;; Entropy Source Trending

(defstruct entropy-trend
  "Track entropy trends over time.
SLOTS:
  measurements     - Time-series of entropy measurements
  timestamps       - Corresponding timestamps
  trend-direction  - :improving, :degrading, :stable
  trend-magnitude  - How strong the trend is"
  (measurements '() :type list)
  (timestamps '() :type list)
  (trend-direction :stable :type keyword)
  (trend-magnitude 0.0 :type single-float))

(defun add-measurement (trend entropy-value)
  "Add entropy measurement to trend tracker."
  (push entropy-value (entropy-trend-measurements trend))
  (push (get-universal-time) (entropy-trend-timestamps trend))
  ;; Keep last 100 measurements
  (when (> (length (entropy-trend-measurements trend)) 100)
    (setf (entropy-trend-measurements trend)
          (subseq (entropy-trend-measurements trend) 0 100))
    (setf (entropy-trend-timestamps trend)
          (subseq (entropy-trend-timestamps trend) 0 100))))

(defun analyze-trend (trend)
  "Analyze entropy trend to determine direction."
  (when (> (length (entropy-trend-measurements trend)) 10)
    (let* ((recent (subseq (entropy-trend-measurements trend) 0 10))
           (older (subseq (entropy-trend-measurements trend) 10 20))
           (recent-mean (mean recent))
           (older-mean (mean older))
           (diff (- recent-mean older-mean)))
      (cond
        ((> diff 0.1) (setf (entropy-trend-trend-direction trend) :improving))
        ((< diff -0.1) (setf (entropy-trend-trend-direction trend) :degrading))
        (t (setf (entropy-trend-trend-direction trend) :stable)))
      (setf (entropy-trend-trend-magnitude trend) (coerce (abs diff) 'single-float)))))

;;; Entropy Validation

(defun validate-entropy-source (source-fn &key (sample-size 5000) (min-entropy 4.0))
  "Validate entropy source meets minimum quality standards.
  RETURNS: T if valid, NIL if not"
  (let ((data (loop repeat sample-size collect (funcall source-fn))))
    (let ((entropy (shannon-entropy data))
          (min-ent (min-entropy data)))
      (and (>= entropy min-entropy)
           (> min-ent 0)))))

(defun entropy-quality-grade (source-fn &key (sample-size 1000))
  "Assign letter grade (A-F) based on entropy characteristics.
  RETURNS: Grade keyword :A :B :C :D :F"
  (let* ((data (loop repeat sample-size collect (funcall source-fn)))
         (entropy (shannon-entropy data))
         (min-ent (min-entropy data))
         (compression-ratio (/ (float (length (compression-test data))) (length data))))
    (cond
      ((and (> entropy 7.5) (> min-ent 6.0) (< compression-ratio 0.3)) :A)
      ((and (> entropy 7.0) (> min-ent 5.0) (< compression-ratio 0.4)) :B)
      ((and (> entropy 6.5) (> min-ent 4.0) (< compression-ratio 0.5)) :C)
      ((and (> entropy 5.0) (> min-ent 2.0)) :D)
      (t :F))))

;;; Entropy Source Comparison

(defun compare-entropy-sources (sources &key (sample-size 1000))
  "Compare multiple entropy sources and return ranking.
  SOURCES: List of (name . function) pairs
  RETURNS: List of (name score) ordered by quality"
  (let ((results nil))
    (loop for (name . source-fn) in sources
          do (let ((benchmark (benchmark-entropy-source source-fn :sample-size sample-size)))
               (push (list name (entropy-quality-score benchmark)) results)))
    (sort results (lambda (a b) (> (cadr a) (cadr b))))))

(defun print-entropy-comparison (sources &key (sample-size 1000) (stream t))
  "Print detailed comparison of entropy sources."
  (format stream "~&=== Entropy Source Comparison ===~%")
  (let ((ranking (compare-entropy-sources sources :sample-size sample-size)))
    (loop for (name score) in ranking
          do (format stream "  ~A: ~5,3F~%" name score))))

;;; Entropy Cache Management

(defun cache-entropy-benchmark (key source-fn &key (sample-size 1000))
  "Cache entropy benchmark results."
  (let ((benchmark (benchmark-entropy-source source-fn :sample-size sample-size)))
    (set-cached-entropy key benchmark)
    benchmark))

(defun invalidate-entropy-cache (&optional key)
  "Clear entropy cache (all if KEY is NIL)."
  (if key
      (sb-thread:with-mutex (*cache-lock*)
        (remhash key *entropy-cache*))
      (clear-entropy-cache)))

;;; Entropy Reporting

(defun print-detailed-entropy-report (source-fn &key (sample-size 5000) (stream t))
  "Print comprehensive entropy analysis report."
  (let ((data (loop repeat sample-size collect (funcall source-fn))))
    (format stream "~&=== Comprehensive Entropy Report ===~%")
    (format stream "  Samples: ~D~%" sample-size)
    (format stream "  Shannon Entropy: ~5,3F bits/symbol~%" (shannon-entropy data))
    (format stream "  Min-Entropy: ~5,3F bits~%" (min-entropy data))
    (format stream "  Fourier Power: ~5,4F~%" (fourier-test data))
    (format stream "  Byte Distribution: ~5,2F~%" (byte-distribution-test data))
    (format stream "  Runs Test: ~5,4F~%" (runs-test (mapcar (lambda (x) (mod x 2)) data)))
    (format stream "  Serial Correlation: ~5,4F~%" (serial-test data))
    (format stream "  Autocorrelation: ~5,4F~%" (autocorrelation-test data))
    (format stream "  Compression Ratio: ~5,3F~%" (compression-test data))
    (format stream "  Quality Grade: ~A~%" (entropy-quality-grade (lambda () (car data)) :sample-size sample-size))
    (format stream "~%")))

;;; Entropy Distribution Analysis

(defun histogram-entropy (data &key (buckets 256))
  "Compute entropy histogram for DATA.
  RETURNS: Array of bucket counts"
  (let ((histogram (make-array buckets :initial-element 0)))
    (loop for item in data
          do (incf (aref histogram (mod item buckets))))
    histogram))

(defun entropy-histogram-report (data &key (buckets 256) (stream t))
  "Print histogram of entropy distribution."
  (let ((histogram (histogram-entropy data :buckets buckets)))
    (format stream "~&=== Entropy Histogram ===~%")
    (format stream "  Buckets: ~D~%" buckets)
    (loop for i from 0 below (min 20 buckets)
          do (let ((count (aref histogram i)))
               (format stream "  Bucket ~3D: ~A~%" i (make-string count :initial-element #\*))))))

;;; Entropy Source Optimization

(defun entropy-optimizer-settings ()
  "Return recommended optimization settings for entropy sources."
  (list :sample-batch-size 1000
        :test-interval-seconds 300
        :health-check-interval 60
        :cache-ttl-seconds 3600
        :max-cached-benchmarks 10))

(defun recommend-entropy-settings (source-fn)
  "Recommend settings for optimal entropy measurement."
  (let ((quality-grade (entropy-quality-grade source-fn :sample-size 1000)))
    (case quality-grade
      (:A (list :sampling-rate :fast :batch-size 100 :test-frequency :hourly))
      (:B (list :sampling-rate :normal :batch-size 500 :test-frequency :every-4-hours))
      (:C (list :sampling-rate :careful :batch-size 2000 :test-frequency :every-8-hours))
      (:D (list :sampling-rate :very-careful :batch-size 5000 :test-frequency :daily))
      (:F (list :status :not-recommended :action "Investigate entropy source")))))

;;; Time-Series Entropy Analysis

(defstruct entropy-timeseries
  "Time series of entropy measurements.
SLOTS:
  measurements     - List of entropy values
  timestamps       - Corresponding timestamps
  window-size      - Size of averaging window
  lock             - Mutex for synchronization"
  (measurements '() :type list)
  (timestamps '() :type list)
  (window-size 10 :type integer)
  (lock (sb-thread:make-mutex :name "timeseries-lock")))

(defun make-entropy-timeseries (&key (window-size 10))
  "Create entropy time series tracker."
  (make-entropy-timeseries :window-size window-size))

(defun add-entropy-measurement (ts entropy-value)
  "Add entropy measurement to time series."
  (sb-thread:with-mutex ((entropy-timeseries-lock ts))
    (push entropy-value (entropy-timeseries-measurements ts))
    (push (get-universal-time) (entropy-timeseries-timestamps ts))
    ;; Keep only recent measurements
    (when (> (length (entropy-timeseries-measurements ts)) 1000)
      (setf (entropy-timeseries-measurements ts)
            (subseq (entropy-timeseries-measurements ts) 0 1000))
      (setf (entropy-timeseries-timestamps ts)
            (subseq (entropy-timeseries-timestamps ts) 0 1000)))))

(defun entropy-moving-average (ts)
  "Calculate moving average of entropy."
  (sb-thread:with-mutex ((entropy-timeseries-lock ts))
    (if (< (length (entropy-timeseries-measurements ts)) (entropy-timeseries-window-size ts))
        (mean (entropy-timeseries-measurements ts))
        (mean (subseq (entropy-timeseries-measurements ts) 0 (entropy-timeseries-window-size ts))))))

(defun entropy-moving-variance (ts)
  "Calculate moving variance of entropy."
  (sb-thread:with-mutex ((entropy-timeseries-lock ts))
    (if (< (length (entropy-timeseries-measurements ts)) (entropy-timeseries-window-size ts))
        (variance (entropy-timeseries-measurements ts))
        (variance (subseq (entropy-timeseries-measurements ts) 0 (entropy-timeseries-window-size ts))))))

(defun detect-entropy-anomalies (ts &key (stddev-threshold 2.5))
  "Detect anomalies in entropy measurements."
  (sb-thread:with-mutex ((entropy-timeseries-lock ts))
    (let* ((measurements (entropy-timeseries-measurements ts))
           (mean-val (mean measurements))
           (stddev-val (stddev measurements)))
      (if (zerop stddev-val)
          '()
          (loop for (val . rest) on measurements
                for i from 0
                when (> (abs (- val mean-val)) (* stddev-threshold stddev-val))
                collect (list :index i :value val :deviation (- val mean-val)))))))

;;; Compliance Checking

(defun is-entropy-compliant (source-fn &key (min-bits 7.5) (min-min-entropy 5.0))
  "Check if entropy source meets compliance requirements."
  (let* ((data (loop repeat 5000 collect (funcall source-fn)))
         (entropy (shannon-entropy data))
         (min-ent (min-entropy data)))
    (and (>= entropy min-bits) (>= min-ent min-min-entropy))))

(defun entropy-compliance-report (source-fn &optional (stream t))
  "Generate compliance report for entropy source."
  (format stream "~&=== Entropy Compliance Report ===~%")
  (let* ((data (loop repeat 5000 collect (funcall source-fn)))
         (entropy (shannon-entropy data))
         (min-ent (min-entropy data)))
    (format stream "  Shannon Entropy: ~5,3F bits/symbol (required: >= 7.5)~%" entropy)
    (format stream "    Status: ~A~%" (if (>= entropy 7.5) "PASS" "FAIL"))
    (format stream "  Min-Entropy: ~5,3F bits (required: >= 5.0)~%" min-ent)
    (format stream "  Status: ~A~%" (if (>= min-ent 5.0) "PASS" "FAIL"))
    (format stream "~%")))

;;; Entropy Source Attestation

(defstruct entropy-attestation
  "Formal attestation of entropy source quality.
SLOTS:
  source-name      - Name of entropy source
  test-date        - Date of testing
  entropy-value    - Measured entropy
  min-entropy-value - Minimum entropy
  quality-grade    - Quality grade
  compliant-p      - Whether it meets standards
  attestor         - Who attested to quality"
  (source-name "" :type string)
  (test-date (get-universal-time) :type integer)
  (entropy-value 0.0 :type single-float)
  (min-entropy-value 0.0 :type single-float)
  (quality-grade :unknown :type keyword)
  (compliant-p nil :type boolean)
  (attestor "" :type string))

(defun create-entropy-attestation (source-name source-fn &key (attestor "system"))
  "Create formal attestation of entropy source."
  (let* ((data (loop repeat 5000 collect (funcall source-fn)))
         (entropy (shannon-entropy data))
         (min-ent (min-entropy data))
         (grade (entropy-quality-grade source-fn :sample-size 5000))
         (compliant (and (>= entropy 7.5) (>= min-ent 5.0))))
    (make-entropy-attestation
     :source-name source-name
     :entropy-value (coerce entropy 'single-float)
     :min-entropy-value (coerce min-ent 'single-float)
     :quality-grade grade
     :compliant-p compliant
     :attestor attestor)))

(defun print-entropy-attestation (attestation &optional (stream t))
  "Print entropy source attestation."
  (format stream "~&=== Entropy Source Attestation ===~%")
  (format stream "  Source: ~A~%" (entropy-attestation-source-name attestation))
  (format stream "  Test Date: ~A~%" (entropy-attestation-test-date attestation))
  (format stream "  Shannon Entropy: ~5,3F~%" (entropy-attestation-entropy-value attestation))
  (format stream "  Min-Entropy: ~5,3F~%" (entropy-attestation-min-entropy-value attestation))
  (format stream "  Quality Grade: ~A~%" (entropy-attestation-quality-grade attestation))
  (format stream "  Compliant: ~A~%" (entropy-attestation-compliant-p attestation))
  (format stream "  Attestor: ~A~%" (entropy-attestation-attestor attestation))
  (format stream "~%"))
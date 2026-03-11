# cl-entropy-health

NIST 800-90B entropy health testing for Common Lisp with zero external dependencies.

## Installation

```lisp
(asdf:load-system :cl-entropy-health)
```

## API

- `(estimate-entropy data)` - Estimate min-entropy of byte sequence
- `(min-entropy data)` - Calculate min-entropy in bits per byte
- `(collision-test data)` - Perform collision test
- `(compression-test data)` - Perform compression test
- `(runs-test data)` - Perform runs test
- `(excursion-test data)` - Perform excursion test
- `(entropy-source-healthy-p data &key threshold)` - Check if entropy source is healthy

## Example

```lisp
(cl-entropy-health:entropy-source-healthy-p random-bytes :threshold 6.0) ; => T
(cl-entropy-health:min-entropy random-bytes) ; => 7.89 (bits per byte)
```

## License

BSD-3-Clause - Parkian Company LLC 2024-2026

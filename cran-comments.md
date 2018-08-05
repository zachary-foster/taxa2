## Test environments

* local ubuntu 16.04, R 3.4.4
* ubuntu 14.04 (on travis-ci), R 3.5.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes


## Reverse dependencies

### metacoder

Metacoder passes all checks with no notes, warnings, or errors, except a few tests fail.

The warning "'min' arg is deprecated. Use 'lower' instead." is due to a change in the `GA` package, where an argument was renamed, but works the same.
It has been fixed in the development version of `metacoder` and does not affect the functionality of the CRAN version.

The "did not produce any warnings." errors are because `taxa` used to produce warnings in some cases, but these were removed since I found out that it was not actually a problem.

This version of taxa being submitted should be fully compatible with the current CRAN version of metacoder, even though the tests fail.

```
Testing metacoder
✔ | OK F W S | Context
✔ | 40       | Calculations [35.1 s]
✔ |  3   2   | Tree plotting [6.0 s]
─────────────────────────────────────────────────────────────────────────────
test--heat_tree.R:12: warning: basic tree plotting works
'min' arg is deprecated. Use 'lower' instead.

test--heat_tree.R:12: warning: basic tree plotting works
'max' arg is deprecated. Use 'upper' instead.
─────────────────────────────────────────────────────────────────────────────
✖ | 51 3     | Input parsing [4.1 s]
─────────────────────────────────────────────────────────────────────────────
test--parsers_and_writers.R:78: failure: Mothur classify.seqs *.tax.summary  detailed parsing
`result <- parse_mothur_tax_summary(text = raw_data)` did not produce any warnings.

test--parsers_and_writers.R:79: failure: Mothur classify.seqs *.tax.summary  detailed parsing
`result_from_file <- parse_mothur_tax_summary(file = "example_data/mothur_summary.txt")` did not produce any warnings.

test--parsers_and_writers.R:97: failure: Mothur classify.seqs *.tax.summary simple parsing
`result <- parse_mothur_tax_summary(text = raw_data)` did not produce any warnings.
─────────────────────────────────────────────────────────────────────────────
✔ | 19       | Simulated PCR [1.3 s]

══ Results ══════════════════════════════════════════════════════════════════
Duration: 46.9 s

OK:       113
Failed:   3
Warnings: 2
Skipped:  0
```
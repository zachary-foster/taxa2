taxa
====



[![Build Status](https://travis-ci.org/ropenscilabs/taxa.svg?branch=master)](https://travis-ci.org/ropenscilabs/taxa)

taxon classes for R

> NOTE: This package may not end up being a separate package, but it may. It's a space to explore taxonomic classes for R

## use cases

* use in [binomen](https://github.com/ropensci/binomen):
    * if this pkg does classes, `binomen` can focus on [verbs](https://github.com/ropensci/binomen#verbs), e.g., manipulating taxonomic classes, doing `split-apply-combine` type things
* use in [taxize](https://github.com/ropensci/taxize):
    * as we don't want to break things, probably ideal to have coerion fxns, e.g., `as.taxon()`, which will convert e.g., the output of `get_uid()` to a `taxa` taxonomic class, which we can then go dowstream and do things with (i.e., whatever we build on top of the classes)
    * Or we could even have output of `get_*()` functions do coercion to `taxa` classes on output since they are just simple S3 classes without print methods right now
* use in [metacoder](https://github.com/grunwaldlab/metacoder): `taxa` will provide the classes used in `metacoder` (the `taxmap` class was originally from `metacoder`) once the `taxa` package is mature.

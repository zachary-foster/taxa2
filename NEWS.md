Current
=======

### Improvements

* `parse_tax_data` can now incorperate rank information which can be accessed by `result$taxon_ranks()` ([issue #113](https://github.com/ropensci/taxa/issues/113)).
* `taxmap` print methods now have more information.

### Bug fixes

* `filter_taxa` now has better error messages for invalid inputs ([issue #117](https://github.com/ropensci/taxa/issues/117)).
* Fix a bug that caused an error in `filter_taxa` when no taxa pass filter ([issue #116](https://github.com/ropensci/taxa/issues/116)).

taxa 0.2.0
==========

### Bug fixes

* Fixed a few problems with using duplicated inputs to `subset` ([issue #88](https://github.com/ropensci/taxa/issues/85), [issue #89](https://github.com/ropensci/taxa/issues/85))
* Fixed a bug that caused an error when using unnamed vectors ([issue #86](https://github.com/ropensci/taxa/issues/86))
* Fixed a bug that prevents using sequence accession numbers ([issue #85](https://github.com/ropensci/taxa/issues/85))
* Fixed bug in `lookup_tax_data` and `extract_tax_data` that caused an error when one of the queries failed too download.
* Fixed bug that caused "data" argument of `obs_apply` to not work when passed as a varaible ([issue #97](https://github.com/ropensci/taxa/issues/97))

### Improvements

* Added `map_data_` for mapping without using NSE.
* Make default dataset for `n_obs` and `n_obs_1` and make them available for NSE ([issue #91](https://github.com/ropensci/taxa/issues/91)
* `parse_tax_data`/`extract_tax_data` can now parse things like `phylum;Nitrosopumilales;order;Nitrosopumilaceae;family;` and split out the rank and taxon names by using multiple matches to the `class_regex` when `class_sep` is NULL. 
* `extract_tax_data` now gives warnings if a regex does not match.
* Added `n_supertaxa_1` function to get number of immediate supertaxa (always 1 or 0).
* Added `branches` function to go with `roots`, `leaves`, and `stems`. ([issue #56](https://github.com/ropensci/taxa/issues/56))
* Added `internodes` and `is_internode` functions to go with `roots`, `leaves`, `branches`, and `stems`. USeful for removing uninformative taxonomic ranks/taxa.
* Started to incorporate ability for `taxon`, `taxon_name`, `taxon_id`, `taxon_rank`, and `taxa` to handle `NULL` inputs as first class citizens to handle cases when you have essentially a blank taxon (use case comes from `taxize` package) [#95](https://github.com/ropensci/taxa/issues/95) [#107](https://github.com/ropensci/taxa/issues/107)
* data parsers: Put long, often unused columns last ([issue #93](https://github.com/ropensci/taxa/issues/93))
* When parsing classifications that have per-taxon info add input id column ([issue #92](https://github.com/ropensci/taxa/issues/92))
* New function `classification` as an abstraction to get either hierarchy of taxon indexes, names, or ids ([issue #57](https://github.com/ropensci/taxa/issues/57))
* New function `get_data_frame` for both `Taxonomy` and `Taxmap` objects that wraps around `get_data` to coerce into a `data.frame`. ([issue #58](https://github.com/ropensci/taxa/issues/58)) ([PR #105](https://github.com/ropensci/taxa/issues/105))

### Changes

* In the output of the taxmap parsing functions like `parse_tax_data`, I moved "taxon_id" and "input_index" columns to front and "input" to rear. Also "tax_data" now comes before "class_data".

taxa 0.1.0
==========

### NEW FEATURES

* Released to CRAN.

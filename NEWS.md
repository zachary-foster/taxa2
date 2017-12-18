taxa [current]
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

### Changes

* In the output of the taxmap parsing functions like `parse_tax_data`, I moved "taxon_id" and "input_index" columns to front and "input" to rear. Also "tax_data" now comes before "class_data".

taxa 0.1.0
==========

### NEW FEATURES

* Released to CRAN.

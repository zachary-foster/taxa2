taxa
====



[![Build Status](https://travis-ci.org/ropenscilabs/taxa.svg?branch=master)](https://travis-ci.org/ropenscilabs/taxa)
[![codecov](https://codecov.io/gh/ropenscilabs/taxa/branch/master/graph/badge.svg)](https://codecov.io/gh/ropenscilabs/taxa)
[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

taxon classes for R

`taxa` defines taxonomic classes for R. The goal is to use these classes as low 
level fundamental taxonomic classes that other R packages can build on and use.

There are two distinct sets of classes in `taxa`:

* so called `taxa` classes that are concerned only with taxonomic information
* so called `taxmap` classes that are concerned with combined taxonomic and 
other data (e.g., molecular sequences)

## use cases

* use in [binomen](https://github.com/ropensci/binomen):
    * if this pkg does classes, `binomen` can focus on [verbs](https://github.com/ropensci/binomen#verbs), e.g., manipulating taxonomic classes, doing `split-apply-combine` type things
* use in [taxize](https://github.com/ropensci/taxize):
    * as we don't want to break things, probably ideal to have coerion fxns, e.g., `as.taxon()`, which will convert e.g., the output of `get_uid()` to a `taxa` taxonomic class, which we can then go dowstream and do things with (i.e., whatever we build on top of the classes)
    * Or we could even have output of `get_*()` functions do coercion to `taxa` classes on output since they are just simple S3 classes without print methods right now
* use in [metacoder](https://github.com/grunwaldlab/metacoder): ???

## Installation


```r
devtools::install_github("ropenscilabs/taxa")
```


```r
library("taxa")
```

## taxa classes

### taxon components

database


```r
(x <- taxon_database(
  "ncbi",
  "http://www.ncbi.nlm.nih.gov/taxonomy",
  "NCBI Taxonomy Database",
  "*"
))
#> <database> ncbi
#>   url: http://www.ncbi.nlm.nih.gov/taxonomy
#>   description: NCBI Taxonomy Database
#>   id regex: *
x$name
#> [1] "ncbi"
x$url
#> [1] "http://www.ncbi.nlm.nih.gov/taxonomy"
```

rank


```r
taxon_rank("species")
#> <TaxonRank> species
#>   database: none
```

name


```r
taxon_name("Poa")
#> <TaxonName> Poa
#>   database: none
```

id


```r
taxon_id(12345)
#> <TaxonId> 12345
#>   database: none
```

### taxon


```r
(x <- taxon(
  name = taxon_name("Poa annua"),
  rank = taxon_rank("species"),
  id = taxon_id(93036)
))
#> <Taxon>
#>   name: Poa annua
#>   rank: species
#>   id: 93036
#>   authority: none
```

### taxa


```r
(x <- taxon(
  name = taxon_name("Poa annua"),
  rank = taxon_rank("species"),
  id = taxon_id(93036)
))
#> <Taxon>
#>   name: Poa annua
#>   rank: species
#>   id: 93036
#>   authority: none
taxa_(x, x, x)
#> <taxa> 
#>   no. taxa:  3 
#>   Poa annua / species / 93036 
#>   Poa annua / species / 93036 
#>   Poa annua / species / 93036
```

## taxamap classes

### Filter

Remove singleton taxa, but reassign singletons to supertaxa that pass filter


```r
filter_taxa(ex_taxmap, 1:3)
#> <Taxmap>
#>   3 taxa: 1. Mammalia, 2. Plantae, 3. Felidae
#>   3 edges: NA->1, NA->2, 1->3
#>   2 data sets:
#>     info:
#>       # A tibble: 6 × 4
#>           name n_legs dangerous taxon_id
#>         <fctr>  <dbl>     <lgl>    <chr>
#>       1  tiger      4      TRUE        3
#>       2    cat      4     FALSE        3
#>       3   mole      4     FALSE        1
#>       # ... with 3 more rows
#>     phylopic_ids:  e148eabb-f138-43c6-b1e4-5cda2180485a ... 63604565-0406-460b-8cb8-1abe954b3f3a
#>   1 functions:
#>  reaction
```

### Mutate

Add one or more taxon columns


```r
mutate_obs(ex_taxmap, "info",
           new_col = "Im new",
           newer_col = paste0(new_col, "er!"))
#> <Taxmap>
#>   17 taxa: 1. Mammalia ... 16. lycopersicum, 17. tuberosum
#>   17 edges: NA->1, NA->2, 1->3 ... 9->14, 10->15, 11->16, 11->17
#>   2 data sets:
#>     info:
#>       # A tibble: 6 × 6
#>           name n_legs dangerous taxon_id new_col newer_col
#>         <fctr>  <dbl>     <lgl>    <chr>   <chr>     <chr>
#>       1  tiger      4      TRUE       12  Im new Im newer!
#>       2    cat      4     FALSE       13  Im new Im newer!
#>       3   mole      4     FALSE       14  Im new Im newer!
#>       # ... with 3 more rows
#>     phylopic_ids:  e148eabb-f138-43c6-b1e4-5cda2180485a ... 63604565-0406-460b-8cb8-1abe954b3f3a
#>   1 functions:
#>  reaction
```

### Select 

Select taxon columns


```r
select_obs(ex_taxmap, "info", dangerous)
#> <Taxmap>
#>   17 taxa: 1. Mammalia ... 16. lycopersicum, 17. tuberosum
#>   17 edges: NA->1, NA->2, 1->3 ... 9->14, 10->15, 11->16, 11->17
#>   2 data sets:
#>     info:
#>       # A tibble: 6 × 2
#>         taxon_id dangerous
#>            <chr>     <lgl>
#>       1       12      TRUE
#>       2       13     FALSE
#>       3       14     FALSE
#>       # ... with 3 more rows
#>     phylopic_ids:  e148eabb-f138-43c6-b1e4-5cda2180485a ... 63604565-0406-460b-8cb8-1abe954b3f3a
#>   1 functions:
#>  reaction
```

### Arrange

Sort by taxon name alphabetically


```r
arrange_taxa(ex_taxmap, desc(ex_taxmap$taxon_names()))
#> <Taxmap>
#>   17 taxa: 1. Mammalia ... 16. lycopersicum, 17. tuberosum
#>   17 edges: 9->14, 11->17, 7->12 ... 1->5, 3->8, 1->3, 8->13
#>   2 data sets:
#>     info:
#>       # A tibble: 6 × 4
#>           name n_legs dangerous taxon_id
#>         <fctr>  <dbl>     <lgl>    <chr>
#>       1  tiger      4      TRUE       12
#>       2    cat      4     FALSE       13
#>       3   mole      4     FALSE       14
#>       # ... with 3 more rows
#>     phylopic_ids:  e148eabb-f138-43c6-b1e4-5cda2180485a ... 63604565-0406-460b-8cb8-1abe954b3f3a
#>   1 functions:
#>  reaction
```

## Contributors

* [Scott Chamberlain](https://github.com/sckott)
* [Zachary Foster](https://github.com/zachary-foster)

## Meta

* Please [report any issues or bugs](https://github.com/ropensci/taxa/issues).
* License: MIT
* Get citation information for `taxa` in R doing `citation(package = 'taxa')`
* Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

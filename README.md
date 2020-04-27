
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hmatch: Tools for cleaning and matching hierarchically-structured data

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis build
status](https://travis-ci.org/epicentre-msf/hmatch.svg?branch=master)](https://travis-ci.org/epicentre-msf/hmatch)
[![Codecov test
coverage](https://codecov.io/gh/epicentre-msf/hmatch/branch/master/graph/badge.svg)](https://codecov.io/gh/epicentre-msf/hmatch?branch=master)
<!-- badges: end -->

An R package for cleaning and matching messy hierarchical data
(e.g. hierarchically-nested adminstrative districts).

### Installation

Install from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("epicentre-msf/hmatch")
```

### Example datasets

The `hmatch` package contains example datasets `ne_raw` (messy
geographical data) and `ne_ref` (reference data derived from a
shapefile), based on a small subset of northeastern North America.

``` r
library(hmatch)

data(ne_raw)
data(ne_ref)

ne_raw # messy raw data
#>             adm0        adm1         adm2
#> 1  united states    new york      suffolk
#> 2         Canada     Ontario         <NA>
#> 3           <NA>        <NA> philadelphia
#> 4  United States        <NA>         York
#> 5           <NA>     NewYork    Jefferson
#> 6           <NA> pensylvania philidelphia
#> 7  united_states        <NA>         king
#> 8           <NA>        <NA>    NJ_Bergen
#> 9           <NA>        <NA>    jeffersen
#> 10          <NA>        <NA>         york

ne_ref # reference data derived from a shapefile
#>    level          adm0         adm1         adm2 hcode
#> 1      0        Canada         <NA>         <NA>   100
#> 2      0 United States         <NA>         <NA>   200
#> 3      1        Canada      Ontario         <NA>   110
#> 4      1 United States     New York         <NA>   220
#> 5      1 United States   New Jersey         <NA>   210
#> 6      1 United States Pennsylvania         <NA>   230
#> 7      2        Canada      Ontario      Toronto   111
#> 8      2        Canada      Ontario         York   112
#> 9      2 United States     New York        Kings   222
#> 10     2 United States     New York      Suffolk   223
#> 11     2 United States     New York    Jefferson   221
#> 12     2 United States   New Jersey       Bergen   211
#> 13     2 United States Pennsylvania Philadelphia   232
#> 14     2 United States Pennsylvania    Jefferson   231
#> 15     2 United States Pennsylvania         York   233
```

### Match messy hierarchically-structured data to a reference dataset

#### Complete matching

Each hierarchical level must match in sequence, with no missing values
below the match level.

``` r
hmatch_complete(ne_raw, ne_ref)
#>             adm0        adm1         adm2 level      ref_adm0 ref_adm1 ref_adm2 hcode
#> 1         Canada     Ontario         <NA>     1        Canada  Ontario     <NA>   110
#> 2  united states    new york      suffolk     2 United States New York  Suffolk   223
#> 3  united_states        <NA>         king    NA          <NA>     <NA>     <NA>  <NA>
#> 4  United States        <NA>         York    NA          <NA>     <NA>     <NA>  <NA>
#> 5           <NA>     NewYork    Jefferson    NA          <NA>     <NA>     <NA>  <NA>
#> 6           <NA> pensylvania philidelphia    NA          <NA>     <NA>     <NA>  <NA>
#> 7           <NA>        <NA>    jeffersen    NA          <NA>     <NA>     <NA>  <NA>
#> 8           <NA>        <NA>    NJ_Bergen    NA          <NA>     <NA>     <NA>  <NA>
#> 9           <NA>        <NA> philadelphia    NA          <NA>     <NA>     <NA>  <NA>
#> 10          <NA>        <NA>         york    NA          <NA>     <NA>     <NA>  <NA>
```

The default join type is “left” (as depicted above), but we can
alternatively specify an “inner” join, which will only return the rows
of `raw` for which a *single positive match* within `ref` is found.

``` r
hmatch_complete(ne_raw, ne_ref, type = "inner")
#>            adm0     adm1    adm2 level      ref_adm0 ref_adm1 ref_adm2 hcode
#> 1        Canada  Ontario    <NA>     1        Canada  Ontario     <NA>   110
#> 2 united states new york suffolk     2 United States New York  Suffolk   223
```

Notice that in the first row (Suffolk County, New York), a match is made
despite the raw and reference data using different cases (lowercase
vs. title case). This is possible because, internally, all input
strings are standardized prior to matching (see
`?string_standardization`).

#### Partial matching

Allows for missing values at one or more level below the match level.

``` r
hmatch_partial(ne_raw, ne_ref)
#>             adm0        adm1         adm2 level      ref_adm0     ref_adm1     ref_adm2 hcode
#> 1         Canada     Ontario         <NA>     1        Canada      Ontario         <NA>   110
#> 2  united states    new york      suffolk     2 United States     New York      Suffolk   223
#> 3  United States        <NA>         York     2 United States Pennsylvania         York   233
#> 4  united_states        <NA>         king    NA          <NA>         <NA>         <NA>  <NA>
#> 5           <NA>     NewYork    Jefferson    NA          <NA>         <NA>         <NA>  <NA>
#> 6           <NA> pensylvania philidelphia    NA          <NA>         <NA>         <NA>  <NA>
#> 7           <NA>        <NA>    jeffersen    NA          <NA>         <NA>         <NA>  <NA>
#> 8           <NA>        <NA>    NJ_Bergen    NA          <NA>         <NA>         <NA>  <NA>
#> 9           <NA>        <NA> philadelphia     2 United States Pennsylvania Philadelphia   232
#> 10          <NA>        <NA>         york     2        Canada      Ontario         York   112
#> 11          <NA>        <NA>         york     2 United States Pennsylvania         York   233
```

#### Partial + fuzzy matching

Partial matching + fuzzy matching based on the
[stringdist](https://github.com/markvanderloo/stringdist) package.

``` r
hmatch_partial(ne_raw, ne_ref, fuzzy = TRUE, max_dist = 2)
#>             adm0        adm1         adm2 level      ref_adm0     ref_adm1     ref_adm2 hcode
#> 1         Canada     Ontario         <NA>     1        Canada      Ontario         <NA>   110
#> 2  united states    new york      suffolk     2 United States     New York      Suffolk   223
#> 3  United States        <NA>         York     2 United States Pennsylvania         York   233
#> 4  united_states        <NA>         king     2 United States     New York        Kings   222
#> 5           <NA>     NewYork    Jefferson     2 United States     New York    Jefferson   221
#> 6           <NA> pensylvania philidelphia     2 United States Pennsylvania Philadelphia   232
#> 7           <NA>        <NA>    jeffersen     2 United States Pennsylvania    Jefferson   231
#> 8           <NA>        <NA>    jeffersen     2 United States     New York    Jefferson   221
#> 9           <NA>        <NA>    NJ_Bergen    NA          <NA>         <NA>         <NA>  <NA>
#> 10          <NA>        <NA> philadelphia     2 United States Pennsylvania Philadelphia   232
#> 11          <NA>        <NA>         york     2        Canada      Ontario         York   112
#> 12          <NA>        <NA>         york     2 United States Pennsylvania         York   233
```

#### Manual matching

Manually-specified matches, linking sets of hierarchical levels in the
raw data to a corresponding code column in the reference data.

``` r
ne_man <- data.frame(adm0 = NA_character_,
                     adm1 = NA_character_,
                     adm2 = "NJ_Bergen",
                     hcode = "211",
                     stringsAsFactors = FALSE)

hmatch_manual(ne_raw, ne_ref, ne_man, code_col = "hcode")
#>             adm0        adm1         adm2 level      ref_adm0   ref_adm1 ref_adm2 hcode
#> 1         Canada     Ontario         <NA>    NA          <NA>       <NA>     <NA>  <NA>
#> 2  united states    new york      suffolk    NA          <NA>       <NA>     <NA>  <NA>
#> 3  united_states        <NA>         king    NA          <NA>       <NA>     <NA>  <NA>
#> 4  United States        <NA>         York    NA          <NA>       <NA>     <NA>  <NA>
#> 5           <NA>     NewYork    Jefferson    NA          <NA>       <NA>     <NA>  <NA>
#> 6           <NA> pensylvania philidelphia    NA          <NA>       <NA>     <NA>  <NA>
#> 7           <NA>        <NA>    jeffersen    NA          <NA>       <NA>     <NA>  <NA>
#> 8           <NA>        <NA>    NJ_Bergen     2 United States New Jersey   Bergen   211
#> 9           <NA>        <NA> philadelphia    NA          <NA>       <NA>     <NA>  <NA>
#> 10          <NA>        <NA>         york    NA          <NA>       <NA>     <NA>  <NA>
```

#### Best-possible matching

Identify potential matches at each successive level, starting with only
the first level, then the first and second level, etc. The best-possible
match then reflects the highest-level that is consistent among all
possible matches to the given row of raw data.

``` r
hmatch_best(raw = ne_raw, ref = ne_ref, fuzzy = TRUE)
#>             adm0        adm1         adm2 level      ref_adm0     ref_adm1     ref_adm2 hcode  match_type
#> 1  united states    new york      suffolk     2 United States     New York      Suffolk   223 best_single
#> 2         Canada     Ontario         <NA>     1        Canada      Ontario         <NA>   110 best_single
#> 3           <NA>        <NA> philadelphia     2 United States Pennsylvania Philadelphia   232 best_single
#> 4  United States        <NA>         York     2 United States Pennsylvania         York   233 best_single
#> 5           <NA>     NewYork    Jefferson     2 United States     New York    Jefferson   221 best_single
#> 6           <NA> pensylvania philidelphia     2 United States Pennsylvania Philadelphia   232 best_single
#> 7  united_states        <NA>         king     2 United States     New York        Kings   222 best_single
#> 8           <NA>        <NA>    NJ_Bergen    NA          <NA>         <NA>         <NA>  <NA>        <NA>
#> 9           <NA>        <NA>    jeffersen     0 United States         <NA>         <NA>   200  best_multi
#> 10          <NA>        <NA>         york    NA          <NA>         <NA>         <NA>  <NA>        <NA>
```

#### The all-strategies approach

Implement all matching strategies in turn, from most to least strict:

1.  (optional) manually-specified matching with `hmatch_manual()`
2.  complete matching with `hmatch_complete()`
3.  partial matching with `hmatch_partial()`
4.  fuzzy partial matching with `hmatch_partial(..., fuzzy = TRUE)`
5.  best-possible matching with
`hmatch_best()`

<!-- end list -->

``` r
hmatch(raw = ne_raw, ref = ne_ref, man = ne_man, fuzzy = TRUE, code_col = "hcode")
#>             adm0        adm1         adm2 level      ref_adm0     ref_adm1     ref_adm2 hcode match_type
#> 1  united states    new york      suffolk     2 United States     New York      Suffolk   223   complete
#> 2         Canada     Ontario         <NA>     1        Canada      Ontario         <NA>   110   complete
#> 3           <NA>        <NA> philadelphia     2 United States Pennsylvania Philadelphia   232    partial
#> 4  United States        <NA>         York     2 United States Pennsylvania         York   233    partial
#> 5           <NA>     NewYork    Jefferson     2 United States     New York    Jefferson   221      fuzzy
#> 6           <NA> pensylvania philidelphia     2 United States Pennsylvania Philadelphia   232      fuzzy
#> 7  united_states        <NA>         king     2 United States     New York        Kings   222      fuzzy
#> 8           <NA>        <NA>    NJ_Bergen     2 United States   New Jersey       Bergen   211     manual
#> 9           <NA>        <NA>    jeffersen     0 United States         <NA>         <NA>   200 best_multi
#> 10          <NA>        <NA>         york    NA          <NA>         <NA>         <NA>  <NA>       <NA>
```

### Generate unique codes for each level in a reference dataset

Functions `hcodes_int()` and `hcodes_str()` can be used to create
integer- or string-based codes, respectively.

``` r
ne_ref$hcode_str <- hcodes_str(ne_ref, pattern = "^adm")
ne_ref
#>    level          adm0         adm1         adm2 hcode                                 hcode_str
#> 1      0        Canada         <NA>         <NA>   100                                    canada
#> 2      0 United States         <NA>         <NA>   200                             united_states
#> 3      1        Canada      Ontario         <NA>   110                           canada__ontario
#> 4      1 United States     New York         <NA>   220                   united_states__new_york
#> 5      1 United States   New Jersey         <NA>   210                 united_states__new_jersey
#> 6      1 United States Pennsylvania         <NA>   230               united_states__pennsylvania
#> 7      2        Canada      Ontario      Toronto   111                  canada__ontario__toronto
#> 8      2        Canada      Ontario         York   112                     canada__ontario__york
#> 9      2 United States     New York        Kings   222            united_states__new_york__kings
#> 10     2 United States     New York      Suffolk   223          united_states__new_york__suffolk
#> 11     2 United States     New York    Jefferson   221        united_states__new_york__jefferson
#> 12     2 United States   New Jersey       Bergen   211         united_states__new_jersey__bergen
#> 13     2 United States Pennsylvania Philadelphia   232 united_states__pennsylvania__philadelphia
#> 14     2 United States Pennsylvania    Jefferson   231    united_states__pennsylvania__jefferson
#> 15     2 United States Pennsylvania         York   233         united_states__pennsylvania__york
```

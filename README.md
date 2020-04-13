
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hmatch: Tools for cleaning and matching hierarchically-structured data

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

An R package for cleaning and matching messy hierarchical data
(e.g. hierarchically-nested adminstrative districts).

## Installation

Install from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("epicentre-msf/hmatch")
```

### Usage

``` r
library(hmatch)

# example datasets (select counties in northereastern North America)
data(ne_raw)
data(ne_ref)
```

#### Generate unique codes for each level in a reference dataset

Functions `hcodes_int()` and `hcodes_str()` can be used to create
integer- or string-based codes, respectively.

``` r
ne_ref$hcode <- hcodes_int(ne_ref, pattern = "^adm")
ne_ref
##    level          adm0         adm1         adm2 hcode
## 1      0        Canada         <NA>         <NA>   100
## 2      0 United States         <NA>         <NA>   200
## 3      1        Canada      Ontario         <NA>   110
## 4      1 United States     New York         <NA>   220
## 5      1 United States   New Jersey         <NA>   210
## 6      1 United States Pennsylvania         <NA>   230
## 7      2        Canada      Ontario      Toronto   111
## 8      2        Canada      Ontario         York   112
## 9      2 United States     New York        Kings   222
## 10     2 United States     New York      Suffolk   223
## 11     2 United States     New York    Jefferson   221
## 12     2 United States   New Jersey       Bergen   211
## 13     2 United States Pennsylvania Philadelphia   232
## 14     2 United States Pennsylvania    Jefferson   231
## 15     2 United States Pennsylvania         York   233
```

#### Match messy hierarchically-structured data to a reference dataset

##### Complete matching

Each hierarchical level must match in sequence.

``` r
hmatch_complete(ne_raw, ne_ref, type = "inner")
##   id          adm0     adm1    adm2 level     bind_adm0 bind_adm1 bind_adm2 hcode
## 1  1 united states new york suffolk     2 United States  New York   Suffolk   223
## 2  2        Canada  Ontario    <NA>     1        Canada   Ontario      <NA>   110
```

##### Partial matching

Allows for missing values at one or more level below the match level.

``` r
hmatch_partial(ne_raw, ne_ref, type = "inner")
##   id          adm0     adm1         adm2 level     bind_adm0    bind_adm1    bind_adm2 hcode
## 1  1 united states new york      suffolk     2 United States     New York      Suffolk   223
## 2  2        Canada  Ontario         <NA>     1        Canada      Ontario         <NA>   110
## 3  3          <NA>     <NA> philadelphia     2 United States Pennsylvania Philadelphia   232
## 4  4 United States     <NA>         York     2 United States Pennsylvania         York   233
```

##### Partial + fuzzy matching

Partial matching + fuzzy matching based on the
[stringdist](https://github.com/markvanderloo/stringdist)
package.

``` r
hmatch_partial(ne_raw, ne_ref, type = "inner", fuzzy = TRUE, max_dist = 2)
##   id          adm0        adm1         adm2 level     bind_adm0    bind_adm1    bind_adm2 hcode
## 1  1 united states    new york      suffolk     2 United States     New York      Suffolk   223
## 2  2        Canada     Ontario         <NA>     1        Canada      Ontario         <NA>   110
## 3  3          <NA>        <NA> philadelphia     2 United States Pennsylvania Philadelphia   232
## 4  4 United States        <NA>         York     2 United States Pennsylvania         York   233
## 5  5          <NA>     NewYork    Jefferson     2 United States     New York    Jefferson   221
## 6  6          <NA> pensylvania philidelphia     2 United States Pennsylvania Philadelphia   232
## 7  7 united_states        <NA>         king     2 United States     New York        Kings   222
```

##### Manual matching

Manually-specified matches, linking sets of hierarchical levels in the
raw data to a corresponding code column in the reference data.

``` r
ne_man <- data.frame(adm0 = NA_character_,
                     adm1 = NA_character_,
                     adm2 = "NJ_Bergen",
                     hcode = "211",
                     stringsAsFactors = FALSE)

hmatch_manual(ne_raw, ne_ref, ne_man, code_col = "hcode", type = "inner")
##   id adm0 adm1      adm2 level     bind_adm0  bind_adm1 bind_adm2 hcode
## 8  8 <NA> <NA> NJ_Bergen     2 United States New Jersey    Bergen   211
```

##### Best-possible matching

Identify potential matches at each successive level, starting with only
the first level, then the first and second level, etc. The best-possible
match then reflects the highest-level that is consistent among all
possible matches to the given row of raw data.

``` r
hmatch_best(raw = ne_raw, ref = ne_ref, fuzzy = TRUE)
##    id          adm0        adm1         adm2 level     bind_adm0    bind_adm1    bind_adm2 hcode  match_type
## 1   1 united states    new york      suffolk     2 United States     New York      Suffolk   223 best_single
## 2   2        Canada     Ontario         <NA>     1        Canada      Ontario         <NA>   110 best_single
## 3   3          <NA>        <NA> philadelphia     2 United States Pennsylvania Philadelphia   232 best_single
## 4   4 United States        <NA>         York     2 United States Pennsylvania         York   233 best_single
## 5   5          <NA>     NewYork    Jefferson     2 United States     New York    Jefferson   221 best_single
## 6   6          <NA> pensylvania philidelphia     2 United States Pennsylvania Philadelphia   232 best_single
## 7   7 united_states        <NA>         king     2 United States     New York        Kings   222 best_single
## 8   8          <NA>        <NA>    NJ_Bergen    NA          <NA>         <NA>         <NA>  <NA>        <NA>
## 9   9          <NA>        <NA>    jeffersen     0 United States         <NA>         <NA>   200  best_multi
## 10 10          <NA>        <NA>         york    NA          <NA>         <NA>         <NA>  <NA>        <NA>
```

##### The all-strategies approach

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
##    id          adm0        adm1         adm2 level     bind_adm0    bind_adm1    bind_adm2 hcode match_type
## 1   1 united states    new york      suffolk     2 United States     New York      Suffolk   223   complete
## 2   2        Canada     Ontario         <NA>     1        Canada      Ontario         <NA>   110   complete
## 3   3          <NA>        <NA> philadelphia     2 United States Pennsylvania Philadelphia   232    partial
## 4   4 United States        <NA>         York     2 United States Pennsylvania         York   233    partial
## 5   5          <NA>     NewYork    Jefferson     2 United States     New York    Jefferson   221      fuzzy
## 6   6          <NA> pensylvania philidelphia     2 United States Pennsylvania Philadelphia   232      fuzzy
## 7   7 united_states        <NA>         king     2 United States     New York        Kings   222      fuzzy
## 8   8          <NA>        <NA>    NJ_Bergen     2 United States   New Jersey       Bergen   211     manual
## 9   9          <NA>        <NA>    jeffersen     0 United States         <NA>         <NA>   200 best_multi
## 10 10          <NA>        <NA>         york    NA          <NA>         <NA>         <NA>  <NA>       <NA>
```

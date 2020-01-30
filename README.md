
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

# example datasets
data(drc_raw)
data(drc_ref)
```

##### Exact matching

Every hierarchical level must match in sequence.

``` r
hmatch_exact(drc_raw, drc_ref, type = "inner")
#>   id      adm1    adm2    adm3  adm4 level bind_adm1 bind_adm2 bind_adm3 bind_adm4                              pcode match_type
#> 1  1 Nord-Kivu Butembo Matanda Ngule     4 Nord-Kivu   Butembo   Matanda     Ngule nord_kivu__butembo__matanda__ngule      exact
#> 2  2 Nord-Kivu   Katwa    <NA>  <NA>     2 Nord-Kivu     Katwa      <NA>      <NA>                   nord_kivu__katwa      exact
```

##### Partial matching

Allows for missing values at one or more level below the match level.

``` r
hmatch_partial(drc_raw, drc_ref, type = "inner")
#>   id      adm1    adm2    adm3      adm4 level bind_adm1 bind_adm2 bind_adm3 bind_adm4                                  pcode match_type
#> 1  1 Nord-Kivu Butembo Matanda     Ngule     4 Nord-Kivu   Butembo   Matanda     Ngule     nord_kivu__butembo__matanda__ngule    partial
#> 2  2 Nord-Kivu   Katwa    <NA>      <NA>     2 Nord-Kivu     Katwa      <NA>      <NA>                       nord_kivu__katwa    partial
#> 3  3      <NA> Butembo    <NA>    Katsya     4 Nord-Kivu   Butembo    Katsya    Katsya     nord_kivu__butembo__katsya__katsya    partial
#> 4  4 Nord-Kivu    <NA>    <NA>    Wayene     4 Nord-Kivu     Katwa  Makerere    Wayene     nord_kivu__katwa__makerere__wayene    partial
#> 5  5      <NA>    <NA>    <NA> Kasongomi     4 Nord-Kivu   Butembo   Matanda Kasongomi nord_kivu__butembo__matanda__kasongomi    partial
```

##### Partial + fuzzy matching

Partial matching + fuzzy matching based on the
[stringdist](https://github.com/markvanderloo/stringdist)
package.

``` r
hmatch_partial(drc_raw, drc_ref, type = "inner", fuzzy = TRUE, max_dist = 2)
#>   id      adm1    adm2     adm3      adm4 level bind_adm1 bind_adm2 bind_adm3 bind_adm4                                  pcode    match_type
#> 1  1 Nord-Kivu Butembo  Matanda     Ngule     4 Nord-Kivu   Butembo   Matanda     Ngule     nord_kivu__butembo__matanda__ngule partial_fuzzy
#> 2  2 Nord-Kivu   Katwa     <NA>      <NA>     2 Nord-Kivu     Katwa      <NA>      <NA>                       nord_kivu__katwa partial_fuzzy
#> 3  3      <NA> Butembo     <NA>    Katsya     4 Nord-Kivu   Butembo    Katsya    Katsya     nord_kivu__butembo__katsya__katsya partial_fuzzy
#> 4  4 Nord-Kivu    <NA>     <NA>    Wayene     4 Nord-Kivu     Katwa  Makerere    Wayene     nord_kivu__katwa__makerere__wayene partial_fuzzy
#> 5  5      <NA>    <NA>     <NA> Kasongomi     4 Nord-Kivu   Butembo   Matanda Kasongomi nord_kivu__butembo__matanda__kasongomi partial_fuzzy
#> 6  6 Nord-Kivu  Katwua Makerere      <NA>     3 Nord-Kivu     Katwa  Makerere      <NA>             nord_kivu__katwa__makerere partial_fuzzy
#> 7  7 nord kivu butemba  matando      <NA>     3 Nord-Kivu   Butembo   Matanda      <NA>            nord_kivu__butembo__matanda partial_fuzzy
```

##### Best-possible matching

Find the best-possible match (i.e. highest resolution), using a variety
of matching strategies implemented in turn.

``` r
hmatch_best(drc_raw, drc_ref, id_col = "id", code_col = "pcode")
#>    id      adm1    adm2     adm3          adm4 bind_adm1 bind_adm2 bind_adm3 bind_adm4                                  pcode    match_type
#> 1   1 Nord-Kivu Butembo  Matanda         Ngule Nord-Kivu   Butembo   Matanda     Ngule     nord_kivu__butembo__matanda__ngule         exact
#> 2   2 Nord-Kivu   Katwa     <NA>          <NA> Nord-Kivu     Katwa      <NA>      <NA>                       nord_kivu__katwa         exact
#> 3   3      <NA> Butembo     <NA>        Katsya Nord-Kivu   Butembo    Katsya    Katsya     nord_kivu__butembo__katsya__katsya       partial
#> 4   4 Nord-Kivu    <NA>     <NA>        Wayene Nord-Kivu     Katwa  Makerere    Wayene     nord_kivu__katwa__makerere__wayene       partial
#> 5   5      <NA>    <NA>     <NA>     Kasongomi Nord-Kivu   Butembo   Matanda Kasongomi nord_kivu__butembo__matanda__kasongomi       partial
#> 6   6 Nord-Kivu  Katwua Makerere          <NA> Nord-Kivu     Katwa  Makerere      <NA>             nord_kivu__katwa__makerere partial_fuzzy
#> 7   7 nord kivu butemba  matando          <NA> Nord-Kivu   Butembo   Matanda      <NA>            nord_kivu__butembo__matanda partial_fuzzy
#> 8   8 Nord-Kivu    <NA>     <NA>       Vutetse Nord-Kivu   Butembo      <NA>      <NA>                     nord_kivu__butembo    best_multi
#> 9   9 Nord-Kivu Butembo   Bagira        Mugaba Nord-Kivu   Butembo      <NA>      <NA>                     nord_kivu__butembo   best_single
#> 10 10      <NA>    <NA>  Vulindi       Lwamiso      <NA>      <NA>      <NA>      <NA>                                   <NA>          <NA>
#> 11 11      <NA>    <NA>     <NA> matanda_ngule      <NA>      <NA>      <NA>      <NA>                                   <NA>          <NA>
```

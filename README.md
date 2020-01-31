
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

#### Generate unique codes for each level in a reference dataset

Functions `hcodes_int()` and `hcodes_str()` can be used to create
integer- or string-based codes, respectively.

``` r
drc_ref$hcode <- hcodes_int(drc_ref, pattern = "^adm")
drc_ref
```

    #>    level      adm1    adm2     adm3      adm4 hcode
    #> 1      1 Nord-Kivu    <NA>     <NA>      <NA>  1000
    #> 2      2 Nord-Kivu Butembo     <NA>      <NA>  1100
    #> 3      2 Nord-Kivu   Katwa     <NA>      <NA>  1200
    #> 4      3 Nord-Kivu Butembo   Katsya      <NA>  1110
    #> 5      3 Nord-Kivu Butembo  Matanda      <NA>  1120
    #> 6      3 Nord-Kivu   Katwa Makerere      <NA>  1210
    #> 7      4 Nord-Kivu Butembo   Katsya    Kahute  1111
    #> 8      4 Nord-Kivu Butembo   Katsya    Katsya  1112
    #> 9      4 Nord-Kivu Butembo   Katsya    Musavu  1113
    #> 10     4 Nord-Kivu Butembo   Katsya   Vutetse  1114
    #> 11     4 Nord-Kivu Butembo  Matanda Kasongomi  1121
    #> 12     4 Nord-Kivu Butembo  Matanda     Ngule  1122
    #> 13     4 Nord-Kivu Butembo  Matanda   Vutetse  1123
    #> 14     4 Nord-Kivu   Katwa Makerere  Makerere  1211
    #> 15     4 Nord-Kivu   Katwa Makerere    Wayene  1212

#### Match messy hierarchically-structured data to a reference dataset

##### Exact matching

Each hierarchical level must match in
    sequence.

``` r
hmatch_exact(drc_raw, drc_ref, type = "inner")
```

    #>   id      adm1    adm2    adm3  adm4 level bind_adm1 bind_adm2 bind_adm3 bind_adm4 hcode
    #> 1  1 Nord-Kivu Butembo Matanda Ngule     4 Nord-Kivu   Butembo   Matanda     Ngule  1122
    #> 2  2 Nord-Kivu   Katwa    <NA>  <NA>     2 Nord-Kivu     Katwa      <NA>      <NA>  1200

##### Partial matching

Allows for missing values at one or more level below the match
    level.

``` r
hmatch_partial(drc_raw, drc_ref, type = "inner")
```

    #>   id      adm1    adm2    adm3      adm4 level bind_adm1 bind_adm2 bind_adm3 bind_adm4 hcode
    #> 1  1 Nord-Kivu Butembo Matanda     Ngule     4 Nord-Kivu   Butembo   Matanda     Ngule  1122
    #> 2  2 Nord-Kivu   Katwa    <NA>      <NA>     2 Nord-Kivu     Katwa      <NA>      <NA>  1200
    #> 3  3      <NA> Butembo    <NA>    Katsya     4 Nord-Kivu   Butembo    Katsya    Katsya  1112
    #> 4  4 Nord-Kivu    <NA>    <NA>    Wayene     4 Nord-Kivu     Katwa  Makerere    Wayene  1212
    #> 5  5      <NA>    <NA>    <NA> Kasongomi     4 Nord-Kivu   Butembo   Matanda Kasongomi  1121

##### Partial + fuzzy matching

Partial matching + fuzzy matching based on the
[stringdist](https://github.com/markvanderloo/stringdist)
package.

``` r
hmatch_partial(drc_raw, drc_ref, type = "inner", fuzzy = TRUE, max_dist = 2)
```

    #>   id      adm1    adm2     adm3      adm4 level bind_adm1 bind_adm2 bind_adm3 bind_adm4 hcode
    #> 1  1 Nord-Kivu Butembo  Matanda     Ngule     4 Nord-Kivu   Butembo   Matanda     Ngule  1122
    #> 2  2 Nord-Kivu   Katwa     <NA>      <NA>     2 Nord-Kivu     Katwa      <NA>      <NA>  1200
    #> 3  3      <NA> Butembo     <NA>    Katsya     4 Nord-Kivu   Butembo    Katsya    Katsya  1112
    #> 4  4 Nord-Kivu    <NA>     <NA>    Wayene     4 Nord-Kivu     Katwa  Makerere    Wayene  1212
    #> 5  5      <NA>    <NA>     <NA> Kasongomi     4 Nord-Kivu   Butembo   Matanda Kasongomi  1121
    #> 6  6 Nord-Kivu  Katwua Makerere      <NA>     3 Nord-Kivu     Katwa  Makerere      <NA>  1210
    #> 7  7 nord kivu butemba  matando      <NA>     3 Nord-Kivu   Butembo   Matanda      <NA>  1120

##### Manual matching

Manually-specified corrections, linking sets of hierarchical levels in
the raw data to a corresponding code column in the reference data.

``` r
drc_man <- data.frame(adm1 = NA_character_,
                      adm2 = NA_character_,
                      adm3 = NA_character_,
                      adm4 = "matanda_ngule",
                      hcode = "1122",
                      stringsAsFactors = FALSE)

hmatch_manual(drc_raw, drc_ref, drc_man, code_col = "hcode", type = "inner")
```

    #>    id adm1 adm2 adm3          adm4 level bind_adm1 bind_adm2 bind_adm3 bind_adm4 hcode
    #> 11 11 <NA> <NA> <NA> matanda_ngule     4 Nord-Kivu   Butembo   Matanda     Ngule  1122

##### Best-possible matching

Find the best-possible match (i.e. highest resolution), using a variety
of matching strategies implemented in
turn.

``` r
hmatch_best(raw = drc_raw, ref = drc_ref, man = drc_man, code_col = "hcode")
```

    #>    id      adm1    adm2     adm3          adm4 level bind_adm1 bind_adm2 bind_adm3 bind_adm4 hcode  match_type
    #> 1   1 Nord-Kivu Butembo  Matanda         Ngule     4 Nord-Kivu   Butembo   Matanda     Ngule  1122       exact
    #> 2   2 Nord-Kivu   Katwa     <NA>          <NA>     2 Nord-Kivu     Katwa      <NA>      <NA>  1200       exact
    #> 3   3      <NA> Butembo     <NA>        Katsya     4 Nord-Kivu   Butembo    Katsya    Katsya  1112     partial
    #> 4   4 Nord-Kivu    <NA>     <NA>        Wayene     4 Nord-Kivu     Katwa  Makerere    Wayene  1212     partial
    #> 5   5      <NA>    <NA>     <NA>     Kasongomi     4 Nord-Kivu   Butembo   Matanda Kasongomi  1121     partial
    #> 6   6 Nord-Kivu  Katwua Makerere          <NA>     3 Nord-Kivu     Katwa  Makerere      <NA>  1210       fuzzy
    #> 7   7 nord kivu butemba  matando          <NA>     3 Nord-Kivu   Butembo   Matanda      <NA>  1120       fuzzy
    #> 8   8 Nord-Kivu    <NA>     <NA>       Vutetse     2 Nord-Kivu   Butembo      <NA>      <NA>  1100  best_multi
    #> 9   9 Nord-Kivu Butembo   Bagira        Mugaba     2 Nord-Kivu   Butembo      <NA>      <NA>  1100 best_single
    #> 10 10      <NA>    <NA>  Vulindi       Lwamiso    NA      <NA>      <NA>      <NA>      <NA>  <NA>        <NA>
    #> 11 11      <NA>    <NA>     <NA> matanda_ngule     4 Nord-Kivu   Butembo   Matanda     Ngule  1122      manual

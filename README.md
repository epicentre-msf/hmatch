
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

An R package for cleaning and matching messy hierarchically-structured
data (e.g. country / region / district / municipality). The general goal
is to match sets of hierarchical values in a raw dataset to
corresponding values within a reference dataset, while accounting for
potential discrepancies such as:

  - variation in character case, punctuation, spacing, use of accents,
    or spelling
  - variation in hierarchical resolution (e.g. some entries specified to
    municipality-level but others only to region)
  - missing values at one or more hierarchical levels
  - values entered at the wrong hierarchical level

### Installation

Install from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("epicentre-msf/hmatch")
```

### Matching strategies

##### Low-level

  - **`hmatch`**: match hierarchical sequences up to the
    highest-resolution level specified within a given row of raw data,
    optionally allowing for missing values below the match level, and
    fuzzy matches (using the
    [stringdist](https://github.com/markvanderloo/stringdist) package)

##### Higher-level

  - **`hmatch_tokens`**: match tokens rather than entire strings to
    allow for variation in multi-term names
  - **`hmatch_permute`**: sequentially permute hierarchical columns to
    allow for values entered at the wrong level
  - **`hmatch_parents`**: match values at a given hierarchical level
    based on shared sets of ‘offspring’
  - **`hmatch_settle`**: try matching at every level and settle for the
    highest-resolution match possible
  - **`hmatch_manual`**: match using a user-supplied dictionary
  - **`hmatch_split`**: implement any other `hmatch_` function
    separately at each hierarchical level, only on unique sequences
  - **`hmatch_composite`**: implement a variety of matching strategies
    in sequence, from most to least strict

### String standardization

Independent of optional fuzzy matching with
[stringdist](https://github.com/markvanderloo/stringdist), `hmatch`
functions use behind-the-scenes string standardization to help account
for variation in character case, punctuation, spacing, or use of accents
between the raw and reference data. E.g.

``` 
              raw_value       reference_value  match
----------------------------------------------------
original:     ILE DE  FRANCE  Île-de-France    FALSE
standardized: ile_de_france   ile_de_france    TRUE
```

Users can choose default standardization (illustrated above), no
standardization, or supply their own preferred function to standardize
strings (e.g. `tolower`).

### Usage

#### Example dataset

The `hmatch` package contains example datasets `ne_raw` (messy
geographical data) and `ne_ref` (reference data derived from a
shapefile), based on a small subset of northeastern North America.

``` r
library(hmatch)

head(ne_raw) # raw messy data
#>      id adm0      adm1         adm2
#> 1 PID01  USA  New York      Suffolk
#> 2 PID02  can   ontario         <NA>
#> 3 PID03  USA  New York Kings County
#> 4 PID04 <NA>      <NA> Philadelphia
#> 5 PID05  USA      <NA>         York
#> 6 PID06  USA new. york    jefferson

head(ne_ref) # reference data derived from shapefile
#>   level adm0         adm1 adm2 hcode
#> 1  adm0  CAN         <NA> <NA>   100
#> 2  adm0  USA         <NA> <NA>   200
#> 3  adm1  CAN      Ontario <NA>   110
#> 4  adm1  USA   New Jersey <NA>   210
#> 5  adm1  USA     New York <NA>   220
#> 6  adm1  USA Pennsylvania <NA>   230
```

#### Example workflow

##### Basic hierarchical matching with `hmatch()`

We’ll start with a simple call to `hmatch` to see which rows can be
matched with no extra magic.

``` r
hmatch(ne_raw, ne_ref, pattern = "^adm")
#>       id adm0           adm1         adm2 level ref_adm0     ref_adm1     ref_adm2 hcode
#> 1  PID01  USA       New York      Suffolk  adm2      USA     New York      Suffolk   227
#> 2  PID02  can        ontario         <NA>  adm1      CAN      Ontario         <NA>   110
#> 3  PID03  USA       New York Kings County  <NA>     <NA>         <NA>         <NA>  <NA>
#> 4  PID04 <NA>           <NA> Philadelphia  adm2      USA Pennsylvania Philadelphia   237
#> 5  PID05  USA           <NA>         York  adm2      USA Pennsylvania         York   238
#> 6  PID06  USA      new. york    jefferson  adm2      USA     New York    Jefferson   222
#> 7  PID07  CAN        Ontario    Peel R.M.  <NA>     <NA>         <NA>         <NA>  <NA>
#> 8  PID08  USA    Pensylvania       Ithaca  <NA>     <NA>         <NA>         <NA>  <NA>
#> 9  PID09  USA       New_York         King  <NA>     <NA>         <NA>         <NA>  <NA>
#> 10 PID10 <NA>           <NA> Bergen, N.J.  <NA>     <NA>         <NA>         <NA>  <NA>
#> 11 PID11  USA   Philadelphia         <NA>  <NA>     <NA>         <NA>         <NA>  <NA>
#> 12 PID12  USA             NJ         <NA>  <NA>     <NA>         <NA>         <NA>  <NA>
#> 13 PID13 <NA>           <NA>    Jeffersen  <NA>     <NA>         <NA>         <NA>  <NA>
#> 14 PID14 <NA>           <NA>         york  adm2      CAN      Ontario         York   115
#> 15 PID14 <NA>           <NA>         york  adm2      USA Pennsylvania         York   238
#> 16 PID15  USA New York State     New York  <NA>     <NA>         <NA>         <NA>  <NA>
```

There are still quite a few unmatched rows, and entry ‘PID14’ actually
matches two different rows within `ref`, so we’ll press on. We can
separate the matched and unmatched rows using inner- and anti-joins
respectively, specifically using the “resolve\_” join type here to only
consider matches that are unique.

``` r
(raw_match1 <- hmatch(ne_raw, ne_ref, pattern = "^adm", type = "resolve_inner"))
#>      id adm0      adm1         adm2 level ref_adm0     ref_adm1     ref_adm2 hcode
#> 1 PID01  USA  New York      Suffolk  adm2      USA     New York      Suffolk   227
#> 2 PID02  can   ontario         <NA>  adm1      CAN      Ontario         <NA>   110
#> 3 PID04 <NA>      <NA> Philadelphia  adm2      USA Pennsylvania Philadelphia   237
#> 4 PID05  USA      <NA>         York  adm2      USA Pennsylvania         York   238
#> 5 PID06  USA new. york    jefferson  adm2      USA     New York    Jefferson   222

(raw_remain1 <- hmatch(ne_raw, ne_ref, pattern = "^adm", type = "resolve_anti"))
#>       id adm0           adm1         adm2
#> 1  PID03  USA       New York Kings County
#> 2  PID07  CAN        Ontario    Peel R.M.
#> 3  PID08  USA    Pensylvania       Ithaca
#> 4  PID09  USA       New_York         King
#> 5  PID10 <NA>           <NA> Bergen, N.J.
#> 6  PID11  USA   Philadelphia         <NA>
#> 7  PID12  USA             NJ         <NA>
#> 8  PID13 <NA>           <NA>    Jeffersen
#> 9  PID14 <NA>           <NA>         york
#> 10 PID15  USA New York State     New York
```

##### Fuzzy matching

Next we’ll add in fuzzy-matching, using the default maximum
string-distance of 1.

``` r
hmatch(raw_remain1, ne_ref, pattern = "^adm", fuzzy = TRUE, type = "inner")
#>      id adm0     adm1      adm2 level ref_adm0     ref_adm1  ref_adm2 hcode
#> 1 PID09  USA New_York      King  adm2      USA     New York     Kings   223
#> 2 PID13 <NA>     <NA> Jeffersen  adm2      USA     New York Jefferson   222
#> 3 PID13 <NA>     <NA> Jeffersen  adm2      USA Pennsylvania Jefferson   235
#> 4 PID14 <NA>     <NA>      york  adm2      CAN      Ontario      York   115
#> 5 PID14 <NA>     <NA>      york  adm2      USA Pennsylvania      York   238
```

Only one additional *unique* match, so we’ll again split and move on.
Note that we’ve been using the `pattern` argument above to specify the
hierarchical columns in `raw` and `ref`, but because the hierarchical
columns have the same names in `raw` and `ref` (and are the only
matching column names), we can drop the `pattern` argument for brevity.

``` r
(raw_match2 <- hmatch(raw_remain1, ne_ref, fuzzy = TRUE, type = "resolve_inner"))
#>      id adm0     adm1 adm2 level ref_adm0 ref_adm1 ref_adm2 hcode
#> 1 PID09  USA New_York King  adm2      USA New York    Kings   223

(raw_remain2 <- hmatch(raw_remain1, ne_ref, fuzzy = TRUE, type = "resolve_anti"))
#>      id adm0           adm1         adm2
#> 1 PID03  USA       New York Kings County
#> 2 PID07  CAN        Ontario    Peel R.M.
#> 3 PID08  USA    Pensylvania       Ithaca
#> 4 PID10 <NA>           <NA> Bergen, N.J.
#> 5 PID11  USA   Philadelphia         <NA>
#> 6 PID12  USA             NJ         <NA>
#> 7 PID13 <NA>           <NA>    Jeffersen
#> 8 PID14 <NA>           <NA>         york
#> 9 PID15  USA New York State     New York
```

##### Tokenized matching

Next let’s try `hmatch_tokens`, which matches based on components of
strings (i.e. tokens) rather than entire strings.

``` r
(raw_match3 <- hmatch_tokens(raw_remain2, ne_ref, type = "resolve_inner"))
#>      id adm0           adm1         adm2 level ref_adm0   ref_adm1 ref_adm2 hcode
#> 1 PID03  USA       New York Kings County  adm2      USA   New York    Kings   223
#> 2 PID07  CAN        Ontario    Peel R.M.  adm2      CAN    Ontario     Peel   113
#> 3 PID10 <NA>           <NA> Bergen, N.J.  adm2      USA New Jersey   Bergen   211
#> 4 PID15  USA New York State     New York  adm2      USA   New York New York   225

(raw_remain3 <- hmatch_tokens(raw_remain2, ne_ref, type = "resolve_anti"))
#>      id adm0         adm1      adm2
#> 1 PID08  USA  Pensylvania    Ithaca
#> 2 PID11  USA Philadelphia      <NA>
#> 3 PID12  USA           NJ      <NA>
#> 4 PID13 <NA>         <NA> Jeffersen
#> 5 PID14 <NA>         <NA>      york
```

##### Permutation matching

If there are any values entered at the wrong hierarchical level, we can
try systematically permuting the hierarchical columns before matching.

``` r
(raw_match4 <- hmatch_permute(raw_remain3, ne_ref, type = "resolve_inner"))
#>      id adm0         adm1 adm2 level ref_adm0     ref_adm1     ref_adm2 hcode
#> 1 PID11  USA Philadelphia <NA>  adm2      USA Pennsylvania Philadelphia   237

(raw_remain4 <- hmatch_permute(raw_remain3, ne_ref, type = "resolve_anti"))
#>      id adm0        adm1      adm2
#> 1 PID08  USA Pensylvania    Ithaca
#> 2 PID12  USA          NJ      <NA>
#> 3 PID13 <NA>        <NA> Jeffersen
#> 4 PID14 <NA>        <NA>      york
```

##### The toughest cases

For the remaining rows that we haven’t yet matched, there a few options.
We could use `hmatch_settle()` to settle for matches below the
highest-resolution level specified within a given row of `raw`. We could
also do some ‘manual’ comparison of the raw and reference datasets and
create a dictionary to recode values within `raw` to match corresponding
entries in `ref`. Here we’ll do both.

``` r
ne_dict <- data.frame(
  value = "NJ",
  replacement = "New Jersey",
  variable = "adm1"
)

(raw_match5 <- hmatch_settle(raw_remain4, ne_ref, dict = ne_dict,
                             fuzzy = TRUE, type = "resolve_inner"))
#>      id adm0        adm1      adm2 level ref_adm0     ref_adm1 ref_adm2 hcode
#> 1 PID08  USA Pensylvania    Ithaca  adm1      USA Pennsylvania     <NA>   230
#> 2 PID12  USA          NJ      <NA>  adm1      USA   New Jersey     <NA>   210
#> 3 PID13 <NA>        <NA> Jeffersen  adm2      USA         <NA>     <NA>   222

(raw_remain5 <- hmatch_settle(raw_remain4, ne_ref, dict = ne_dict,
                              fuzzy = TRUE, type = "resolve_anti"))
#>      id adm0 adm1 adm2
#> 1 PID14 <NA> <NA> york
```

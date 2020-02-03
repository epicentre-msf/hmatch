#' Specifying which columns to match
#'
#' @name specifying_columns
#'
#' @description
#' Within the `hmatch_` group of functions, there are three ways to specify the
#' hierarchical columns to be matched.
#'
#' In all cases, the order of the matched columns must be from the broadest
#' level (lowest-resolution, e.g. country) to the finest level
#' (highest-resolution, e.g. township).
#'
#' **1. Regex pattern** (arguments `pattern_raw` and `pattern_ref`)
#'
#' Arguments `pattern_raw` and `pattern_ref` take regex patterns to match the
#' hierarchical columns in `raw` and `ref`, respectively.
#'
#' For example, if the hierarchical columns in `raw` are *ADM_1*, *ADM_2*, and
#' *ADM_3*, which correspond respectively to columns within `ref` named
#' *REF_ADM_1*, *REF_ADM_2*, and *REF_ADM_3*, then the pattern arguments can be
#' specified as:
#' - `pattern_raw = "^ADM_[[:digit:]]"`
#' - `pattern_ref = "^REF_ADM_[[:digit:]]"`
#'
#' Alternatively, because `pattern_ref` defaults to the same value as
#' `pattern_raw` (unless otherwise specified), one could match the columns in
#' both `raw` and `ref` using simply:
#' - `pattern_raw = "ADM_[[:digit:]]"`
#'
#' However, the user should excercise care to ensure that there are no
#' non-hierarchical columns within `raw` or `ref` that may inadvertently be
#' matched by the given pattern.
#'
#' **2. Named vector** (argument `by`)
#'
#' Alternatively, argument `by` takes a named vector whose elements are the
#' names of the hierarchical columns in `ref` and whose names are the names of
#' the corresponding columns in `raw`.
#'
#' For example, if the hierarchical columns in `raw` are *STATE*, *COUNTY*, and
#' *TOWNSHIP*, which correspond respectively to columns within `ref` named
#' *ADM1*, *ADM2*, and *ADM3*, then argument `by` can be specified as:
#'
#' `by = setNames(c("ADM1", "ADM2", "ADM3"), c("STATE", "COUNTY", "TOWNSHIP"))`
#'
#' **3. (Default) All columns common to `raw` and `ref`**
#'
#' If neither `pattern_` nor `by` are specified (the default), then the
#' hierarchical columns are assumed to be all columns that are common to both
#' `raw` and `ref`.
#'
#' In this case, the user should ensure that the hierarchical columns are
#' already appropriatetly ordered (from broadest to finest) in both the `raw`
#' and `ref` datasets.
#'
NULL



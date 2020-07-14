#' Specifying hierarchical columns with arguments `pattern` or `by`
#'
#' @name specifying_columns
#'
#' @description
#' Within the `hmatch_` group of functions, there are three ways to specify the
#' hierarchical columns to be matched.
#'
#' In all cases, it is assumed that matched columns are already correctly
#' ordered, with the first matched column reflecting the broadest hierarchical
#' level (lowest-resolution, e.g. country) and the last column reflecting the
#' finest level (highest-resolution, e.g. township).
#'
#' @section (1) All column names common to `raw` and `ref`:
#'
#' If neither `pattern` nor `by` are specified (the default), then the
#' hierarchical columns are assumed to be all column names that are common to
#' both `raw` and `ref`.
#'
#' @section (2) Regex pattern:
#'
#' Arguments `pattern` and `pattern_ref` take regex patterns to match the
#' hierarchical columns in `raw` and `ref`, respectively. Argument `pattern_ref`
#' only needs to be specified if it's different from `pattern` (i.e. if the
#' hierarchical columns have different names in `raw` vs. `ref`).
#'
#' For example, if the hierarchical columns in `raw` are "ADM_1", "ADM_2", and
#' "ADM_3", which correspond respectively to columns within `ref` named
#' "REF_ADM_1", "REF_ADM_2", and "REF_ADM_3", then the pattern arguments can be
#' specified as:
#' - `pattern = "^ADM_[[:digit:]]"`
#' - `pattern_ref = "^REF_ADM_[[:digit:]]"`
#'
#' Alternatively, because `pattern_ref` defaults to the same value as
#' `pattern` (unless otherwise specified), one could specify a single regex pattern
#' that matches the hierarchical columns in both `raw` and `ref`, e.g.
#' - `pattern = "ADM_[[:digit:]]"`
#'
#' However, the user should exercise care to ensure that there are no
#' non-hierarchical columns within `raw` or `ref` that may inadvertently be
#' matched by the given pattern.
#'
#' @section (3) Vector of column names:
#'
#' If the hierarchical columns cannot easily be matched with a regex pattern,
#' one can specify the relevant column names in vector form using arguments `by`
#' and `by_ref`. As with `pattern_ref`, argument `by_ref` only needs to be
#' specified if it's different from `by` (i.e. if the hierarchical columns have
#' different names in `raw` vs. `ref`).
#'
#' For example, if the hierarchical columns in `raw` are "state", "county", and
#' "township", which correspond respectively to columns within `ref` named
#' "admin1", "admin2", and "admin3", then the`by` arguments can be specified
#' with:
#'
#' - `by = c("state", "county", "township")`
#' - `by_ref = c("admin1", "admin2", "admin3")`
#'
NULL

#' Dictionary-based recoding of values during hierarchical matching
#'
#' @name dictionary_recoding
#'
#' @description
#' During hierarchical matching with the `hmatch_` group of functions, values
#' within `raw` can be temporarily recoded to match values within `ref` based on
#' a dictionary (argument `dict`) that maps raw values to their desired
#' replacement values (optionally limited to a given hierarchical column).
#'
#' Note that this recoding is done internally, and doesn't actually modify the
#' values of `raw` that are returned (it just enables a match to the proper
#' values of `ref`).
#'
#' For example, if the raw data contains entries of "USA" for variable "adm0",
#' which we know correspond to the value "United States" within the reference
#' data, we can specify a dictionary as follows:
#'
#' `dict <- data.frame(value = "USA",
#'                     replacement = "United States",
#'                     variable = "adm0")`
#'
#' The column names in the dictionary don't actually matter, but the column
#' order must be:
#' 1. value in `raw` to temporarily replace
#' 2. replacement value (to match value in `ref`)
#' 3. (optional) name of hierarchical column in `raw` to recode
#'
#' @section Specifying column(s) to recode:
#'
#' If the dictionary contains only two columns (values and replacements), then
#' all recoding will be applied to every hierarchical column.
#'
#' To apply only a portion of the dictionary to all hierarchical columns (and
#' the rest to specified columns), a user can specify a third dictionary column
#' with values of `<NA>` in rows where the recoding should apply to all
#' hierarchical columns. E.g.
#'
#' `dict <- data.frame(value = c("USA", "Washingtin"
#'                     replace = c("United States", "Washington"),
#'                     variable = c("adm0", NA))`
#'
#' For example, the dictionary above specifies that values of "USA" within
#' column "adm0" will be temporarily replaced with "United States", while values
#' of "Washingtin" within any hierarchical column will be replaced with
#' "Washington".
#'
#' @section String standardization:
#'
#' Note that string standardization as specifed by argument `std_fn` (see
#' \link{string_standardization}) also applies to dictionaries. For example,
#' given the default standardization function which includes
#' case-standardization, a dictionary value of "USA" will match (and therefore
#' recode) `raw` enries "USA" and "usa", but not e.g. "U.S.A.".
#'
NULL


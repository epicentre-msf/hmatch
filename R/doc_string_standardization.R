#' String Standardization
#'
#' @name string_standardization
#'
#' @description
#' Prior to matching raw and reference datasets, one might wish to standardize
#' the strings within the match columns to account for differences in case,
#' punctuation, etc.
#'
#' By default, this standardization is performed with function
#' \code{\link{string_std}}, which implements four transformations:
#' 1. standardize case (`base::tolower`)
#' 2. remove sequences of non-alphanumeric characters at start or end of string
#' 3. replace remaining sequences of non-alphanumeric characters with "_"
#' 4. remove diacritics (`stringi::stri_trans_general`)
#' 5. (optional) convert roman numerals (I, II, ..., XLIX) to arabic (1, 2, ...,
#' 49)
#'
#' Alternatively, the user may provide any function that takes a vector of
#' strings and returns a vector of transformed strings. To omit any
#' transformation, set argument `std_fn = NULL`.
#'
#' Note that the standardized versions of the match columns are never returned.
#' They are used only during matching, and then removed prior to the return.
NULL

#' String standardization prior to matching
#'
#' Standardizes strings prior to performing a match, using three
#' transformations:
#' 1. standardize case (`base::tolower`)
#' 2. replace sequences of space or punctuation characters with "_"
#' 3. remove diacritics (`stringi::stri_trans_general`)
#'
#' @param x a string
#'
#' @return
#' The standardized version of `x`
#'
#' @seealso
#' \link{string_standardization}
#'
#' @examples
#' string_std("United STATES")
#' string_std("R\u00e9publique  d\u00e9mocratique du  Congo")
#'
#' @importFrom stringi stri_trans_general
#' @export string_std
string_std <- function(x) {
  x <- tolower(x)
  x <- gsub("[^[:alnum:]]+", "_", x)
  x <- stringi::stri_trans_general(x, id = "Latin-ASCII")
  x
}

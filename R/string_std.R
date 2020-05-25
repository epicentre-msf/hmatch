#' String standardization prior to matching
#'
#' Standardizes strings prior to performing a match, using four transformations:
#' 1. standardize case (`base::tolower`)
#' 2. remove sequences of non-alphanumeric characters at start or end of string
#' 3. replace remaining sequences of non-alphanumeric characters with "_"
#' 4. remove diacritics (`stringi::stri_trans_general`)
#' 5. (optional) convert roman numerals (I, II, ..., X) to arabic (1, 2, ...,
#' 10)
#'
#' @param x a string
#' @param convert_roman logical indiciating whether to convert roman numerals
#'   (I, II, ..., X) to arabic (1, 2, ..., 10)
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
#' # convert roman numerals to arabic
#' string_std("Mungindu-II (Sud)")
#' string_std("Mungindu-II (Sud)", convert_roman = TRUE)
#'
#' # note the conversion only works if the numeral is separated from other
#' # alphanumeric characters by punctuation or space characters
#' string_std("MunginduII", convert_roman = TRUE) # roman numeral not recognized
#'
#' @importFrom stringi stri_trans_general
#' @export string_std
string_std <- function(x, convert_roman = FALSE) {
  x <- tolower(x)
  x <- gsub("^[^[:alnum:]]+|[^[:alnum:]]+$", "", x)
  x <- gsub("[^[:alnum:]]+", "_", x)
  x <- stringi::stri_trans_general(x, id = "Latin-ASCII")
  if (convert_roman) x <- vapply(x, roman_to_arabic, "", USE.NAMES = FALSE)
  return(x)
}


#' @noRd
roman_to_arabic <- function(x) {
  if (is.na(x)) {
    out <- x
  } else {
    rom <- c("i", "ii", "iii", "iv", "v", "vi", "vii", "viii", "ix", "x")
    xx <- strsplit(x, "_")[[1]]
    xi <- xx %in% rom
    xx[xi] <- match(xx[xi], rom)
    out <- paste(xx, collapse = "_")
  }
  return(out)
}


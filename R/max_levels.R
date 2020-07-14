#' Maximum hierarchical levels
#'
#' Given a data frame with columns specifying hierarchically-nested levels, find
#' the maximum non-missing hierarchical level for each row.
#'
#' @param x a data frame containing hierarchical columns
#' @param pattern regex pattern to match the names of the hierarchical columns
#'   in `ref` (supply either `pattern` *or* `by`)
#' @param by vector giving the names of the hierarchical columns in `ref`
#'   (supply either `pattern` *or* `by`)
#' @param type type of return, either "index" to return integer indices
#'   (starting at 1) or "name" to return column names (as matched by `pattern`
#'   or `by`)
#'
#' @return
#' Vector of indices or names corresponding to the maximum non-missing
#' hierarchical level for each row
#'
#' @examples
#' data(ne_ref)
#'
#' # return integer indices (starting at 1)
#' max_levels(ne_raw, pattern = "^adm")
#'
#' # return column names
#' max_levels(ne_raw, pattern = "^adm", type = "name")
#'
#' @export max_levels
max_levels <- function(x,
                       pattern,
                       by,
                       type = c("index", "name")) {

  ## validate arguments
  type <- match.arg(type)
  by <- select_columns(x, pattern, by)

  if (nrow(x) == 0) {

    if (type == "name") {
      out <- character(0)
    } else {
      out <- integer(0)
    }

  } else {

    m <- !is.na(x[, by, drop = FALSE])
    m <- cbind(rep(TRUE, nrow(m)), m)
    j <- apply(m, 1, function(x) max(which(x))) - 1L

    if (type == "name") {
      names_out <- c(NA_character_, by)
      out <- names_out[j + 1L]
    } else {
      out <- j
    }
  }

  out
}

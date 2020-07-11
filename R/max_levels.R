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
#' @param sort logical indicating whether to alphanumerically sort hierarchical
#'   columns matched by arguments `pattern` or `by` (defaults to `FALSE`)
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
                       pattern = NULL,
                       by = NULL,
                       type = c("index", "name"),
                       sort = FALSE) {

  type <- match.arg(type)

  if (!is.null(pattern) & !is.null(by)) {
    stop("only one of `pattern` or `by` should be provided")
  } else if (is.null(by) & is.null(pattern)) {
    by <- names(x)
  } else if (!is.null(pattern)) {
    by <- grep(pattern, names(x), value = TRUE)
  }

  if (sort) by <- sort(by)

  m <- !is.na(x[, by, drop = FALSE])
  m <- cbind(rep(TRUE, nrow(m)), m)
  j <- apply(m, 1, function(x) max(which(x))) - 1L

  if (type == "name") {
    names_out <- c(NA_character_, by)
    out <- names_out[j + 1L]
  } else {
    out <- j
  }

  out
}

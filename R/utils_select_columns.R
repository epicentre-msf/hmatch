
#' Column matching and validation by regex pattern
#' @noRd
select_pattern <- function(x, pattern) {

  var <- deparse(substitute(pattern))
  by <- grep(pattern, names(x), value = TRUE)

  if (length(by) == 0L) {
    stop("No column names were matched by argument ", backtick(var),
         call. = FALSE)
  }

  by
}


#' Column matching and validation by vector of column names
#' @noRd
select_by <- function(x, by) {

  var <- deparse(substitute(by))
  names_x <- names(x)
  by_no_match <- setdiff(by, names_x)

  if (length(by_no_match) > 0) {
    stop("The following element(s) of argument ", backtick(var),
         " could not be matched to a column name: ", vec_paste_c(by_no_match),
         call. = FALSE)
  }

  by_match <- names_x[names_x %in% by]

  if (!all(by == by_match)) {
    warning("The order of columns within ", backtick(var), " differs from the ",
            "order matched: ", vec_paste_c(by), " vs. ", vec_paste_c(by_match),
            call. = FALSE)
  }

  by
}



#' Helper to match and validate column names specified with pattern or by
#' @noRd
select_columns <- function(x, pattern, by, allow_both_null = FALSE) {

  if (missing(pattern)) pattern <- NULL
  if (missing(by)) by <- NULL

  validate_match_args(pattern, by, allow_both_null = allow_both_null)

  if (!is.null(by)) {
    by <- select_by(x, by)
  } else if (!is.null(pattern)) {
    by <- select_pattern(x, pattern)
  } else {
    by <- names(x)
  }

  by
}


#' @noRd
validate_match_args <- function(pattern, by, allow_both_null = TRUE) {

  ## variable names
  var_pattern <- deparse(substitute(pattern))
  var_by <- deparse(substitute(by))

  ## simple validation: check for both null or both not null
  if (!allow_both_null & is.null(pattern) & is.null(by)) {
    stop("Must provide an argument to match hierarchical column names, either ",
         backtick(var_pattern), " or ", backtick(var_by), call. = FALSE)
  } else if (!is.null(pattern) & !is.null(by)) {
    warning("Arguments ", backtick(var_pattern), " and ", backtick(var_by),
            " are both specified. Ignoring ", backtick(var_pattern), " and ",
            "using ", backtick(var_by), " to match hierarchical column names",
            call. = FALSE)
  }
}


#' Separate a hierarchical code reflecting multiple levels into its constituent
#' parts, with one column for each level
#'
#' @description
#' Separate a data frame column containing hierarchical codes into multiple
#' columns, one for each level within the hierarchical code.
#'
#' Like [`tidyr::separate`] except that successive levels are cumulative rather
#' then independent. E.g. the code "canada__ontario__toronto" would be split
#' into three levels:
#'
#' 1. "canada"
#' 2. "canada__ontario"
#' 3. "canada__ontario__toronto"
#'
#' @param x `data.frame` containing a column with hierarchical codes
#' @param col Name of the column within `x` containing hierarchical codes.
#' @param into Vector of column names to separate `col` into
#' @param sep Separator between levels in the hierarchical codes. Defaults to
#'   "__".
#' @param extra What to do if a hierarchical code contains more levels than are
#'   implied by argument `into`.
#' - "warn" (the default): emit a warning and drop extra values
#' - "drop": drop any extra values without a warning
#' @param remove Logical indicating whether to remove `col` from the output.
#'   Defaults to `FALSE`.
#'
#' @return
#' The original data.frame `x` with additional columns for each level of the
#' hierarchical code
#'
#' @examples
#' data(ne_ref)
#'
#' # generate pcode
#' ne_ref$pcode <- hcodes_str(ne_ref, pattern = "^adm\\d")
#'
#' # separate pcode into constituent levels
#' separate_hcode(
#'   ne_ref,
#'   col = "pcode",
#'   into = c("adm0_pcode", "adm1_pcode", "adm2_pcode")
#' )
#'
#' @importFrom dplyr bind_cols
#' @export separate_hcode
separate_hcode <- function(x,
                           col,
                           into,
                           sep = "__",
                           extra = c("warn", "drop"),
                           remove = FALSE) {

  extra <- match.arg(extra)
  x_separate <- separate_hcode_(x[[col]], sep = "__", into = into, extra = extra)
  out <- dplyr::bind_cols(x, x_separate)
  if (remove) { out <- out[,!names(out) %in% col]}

  out
}


#' @noRd
#' @importFrom stringi stri_sub stri_locate_all
#' @importFrom dplyr bind_rows
#' @importFrom stats setNames
separate_hcode_ <- function(x, sep, into, extra) {

  # locate sep or end of string (eos)
  l_loc <- stringi::stri_locate_all(x, regex = paste0(sep, "|$"))

  # list of starting position(s) for sep/eos in each element of x
  l_sep_start <- lapply(l_loc, function(x) as.integer(x[,1]))

  # separate codes in x into their component levels
  out_l <- mapply(
    function (x, sep_start) stringi::stri_sub(x, 1, sep_start - 1, use_matrix = FALSE),
    x = x,
    sep_start = l_sep_start,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )

  # check for extra levels, and warn if applicable
  extra_levels <- lengths(out_l) > length(into)

  if (any(extra_levels)) {

    n_extra <- sum(extra_levels)
    which_extra <- which(extra_levels)

    if (extra == "warn") {
      warning(
        "Expected ", length(into), " piece(s). ",
        "Additional pieces discarded in ", n_extra, " rows ",
        "[", paste(which_extra, collapse = ", "), "]",
        call. = FALSE
      )
    }
  }

  # remove extra levels and set names based on arg `into`
  out_l <- lapply(
    out_l,
    function (x, into) stats::setNames(x[seq_along(into)], into),
    into = into
  )

  # bind to df, prep empty df if cols otherwise empty
  out <- dplyr::bind_rows(out_l)

  if (ncol(out) == 0L) {
    out <- dplyr::bind_rows(stats::setNames(rep(NA_character_, length(into)), into))[0,]
  }

  # return
  out
}


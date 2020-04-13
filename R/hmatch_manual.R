#' Manual hierarchical matching
#'
#' Match a data.frame with raw, potentially messy hierarchical data (e.g.
#' province, county, township) against a reference dataset, using a dictionary
#' of manually-specified matches.
#'
#' @param raw `data.frame` containing hierarchical columns with raw, potentially
#'   messy data
#' @param ref `data.frame` containing hierarchical columns with reference data
#' @param man `data.frame` of manually-specified matches, relating a given set
#'   of hierarchical values to the code within `ref` to which those values
#'   correspond
#' @param pattern_raw regex pattern to match the hierarchical columns in `raw`
#'   and `man` (see also \link{specifying_columns})
#' @param pattern_ref regex pattern to match the hierarchical columns in `ref`
#'   (see also \link{specifying_columns})
#' @param by named character vector whose elements are the names of the
#'   hierarchical columns in `raw` and `man`, and whose names are the names of
#'   the corresponding columns in `ref` (see also \link{specifying_columns})
#' @param code_col name of the code column containing codes for matching `ref`
#'   and `man`
#' @param type type of join ("inner" or "left") (defaults to "left")
#' @param std_fn Function to standardize strings during matching. Defaults to
#'   \code{\link{string_std}}. Set to `NULL` to omit standardization. See
#'   also \link{string_standardization}.
#'
#' @return A `data.frame` obtained by matching the hierarchical columns in `raw`
#'   and `ref`, based on the matches specified in `man`. If `type == "inner"`,
#'   returns only the rows of `raw` with a single match in `ref`. If `type ==
#'   "left"`, returns all rows of `raw`. If the hierarchical columns within
#'   `ref` have identical names to `raw`, the returned reference columns will be
#'   renamed with prefix "bind_".
#'
#' @examples
#' data(ne_raw)
#' data(ne_ref)
#'
#' # create hcodes in ref
#' ne_ref$hcode <- hcodes_int(ne_ref, pattern = "^adm")
#'
#' # create df mapping sets of raw hierarchical values to codes within ref
#' ne_man <- data.frame(adm0 = NA_character_,
#'                      adm1 = NA_character_,
#'                      adm2 = "NJ_Bergen",
#'                      hcode = "211",
#'                      stringsAsFactors = FALSE)
#'
#' # find manual matches
#' hmatch_manual(ne_raw, ne_ref, ne_man, code_col = "hcode")
#'
#' @importFrom stats setNames
#' @importFrom dplyr left_join
#' @export hmatch_manual
hmatch_manual <- function(raw,
                          ref,
                          man,
                          pattern_raw = NULL,
                          pattern_ref = pattern_raw,
                          by = NULL,
                          code_col,
                          type = "left",
                          std_fn = string_std) {

  if (!is.null(std_fn)) std_fn <- match.fun(std_fn)

  if (code_col %in% names(raw)) {
    warning("Column `code_col` already exists in `raw`, and will be overwritten")
    raw <- raw[!names(raw) %in% code_col]
  }

  list_prep_ref <- prep_ref(raw = raw,
                            ref = ref,
                            pattern_raw = pattern_raw,
                            pattern_ref = pattern_ref,
                            by = by)

  list_prep_man <- prep_ref(raw = raw,
                            ref = man,
                            pattern_raw = pattern_raw,
                            pattern_ref = pattern_ref,
                            by = by)

  ref <- list_prep_ref$ref

  by_raw <- list_prep_ref$by_raw
  by_man <- list_prep_ref$by_raw
  by_join <- list_prep_ref$by_join

  raw_join <- add_join_columns(raw, by_raw, join_cols = by_join, std_fn = std_fn)

  man$TEMP_IS_MATCH <- "MATCH"

  man_join <- add_join_columns(man, by_man, join_cols = by_join, std_fn = std_fn)
  man_ <- unique(man_join[,c(by_join, code_col, "TEMP_IS_MATCH")])

  if (any(duplicated(man_[, by_join, drop = FALSE]))) {
    stop("Duplicated rows in `man` after standardization")
  }

  out <- left_join(raw_join, man_, by = by_join)
  out <- out[, c(names(raw), code_col, "TEMP_IS_MATCH"), drop = FALSE]
  out <- left_join(out, ref, by = code_col)
  out <- out[, c(names(raw), names(ref), "TEMP_IS_MATCH"), drop = FALSE]

  if (type == "inner") {
    out <- out[!is.na(out$TEMP_IS_MATCH),]
  }

  out[,!names(out) %in% "TEMP_IS_MATCH", drop = FALSE]
}


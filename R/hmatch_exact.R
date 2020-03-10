#' Exact hierarchical matching
#'
#' Match a data.frame with raw, potentially messy hierarchical data (e.g.
#' province, county, township) against a reference dataset, using exact
#' matching. "Exact" here means that every level must match in sequence. For
#' example, given raw data with known levels of county and township, but missing
#' the lower-resolution level province, a match will not be made.
#'
#' @param raw `data.frame` containing hierarchical columns with raw, potentially
#'   messy data
#' @param ref `data.frame` containing hierarchical columns with reference data
#' @param pattern_raw regex pattern to match the hierarchical columns in `raw`
#' @param pattern_ref regex pattern to match the hierarchical columns in `ref`
#' @param by named character vector whose elements are the names of the
#'   hierarchical columns in `ref` and whose names are the names of the
#'   corresponding columns in `raw`
#' @param type type of join ("inner" or "left") (defaults to "left")
#' @param std_fn Function to standardize strings during matching. Defaults to
#'   \code{\link{string_std}}. Set to `NULL` to omit standardization. See
#'   also \link{string_standardization}.
#'
#' @return A `data.frame` obtained by matching the hierarchical columns in `raw`
#'   and `ref`. If `type == "inner"`, returns only the rows of `raw` with a
#'   single match in `ref`. If `type == "left"`, returns all rows of `raw`. If
#'   the hierarchical columns within `ref` have identical names to `raw`, the
#'   returned reference columns will be renamed with prefix "bind_".
#'
#' @examples
#' data(drc_raw)
#' data(drc_ref)
#'
#' hmatch_exact(drc_raw, drc_ref, pattern_raw = "adm")
#'
#' @importFrom stats setNames
#' @importFrom dplyr left_join bind_cols
#' @export hmatch_exact
hmatch_exact <- function(raw,
                         ref,
                         pattern_raw = NULL,
                         pattern_ref = pattern_raw,
                         by = NULL,
                         type = "left",
                         std_fn = string_std) {

  if (!is.null(std_fn)) std_fn <- match.fun(std_fn)

  list_prep_ref <- prep_ref(raw = raw,
                            ref = ref,
                            pattern_raw = pattern_raw,
                            pattern_ref = pattern_ref,
                            by = by)

  ref <- list_prep_ref$ref
  by_raw <- list_prep_ref$by_raw
  by_ref <- list_prep_ref$by_ref
  by_join <- list_prep_ref$by_join

  raw_join <- add_join_columns(raw, by_raw, join_cols = by_join, std_fn = std_fn)
  ref_join <- add_join_columns(ref, by_ref, join_cols = by_join, std_fn = std_fn)

  ref_join$TEMP_IS_MATCH <- "MATCH"

  out <- dplyr::left_join(raw_join, ref_join, by = by_join)
  out <- out[,!names(out) %in% by_join, drop = FALSE]

  if (type == "inner") { out <- out[!is.na(out$TEMP_IS_MATCH),] }

  out[,!names(out) %in% "TEMP_IS_MATCH", drop = FALSE]
}

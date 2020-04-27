#' Complete hierarchical matching
#'
#' Match a data.frame with raw, potentially messy hierarchical data (e.g.
#' province, county, township) against a reference dataset, using complete
#' matching. "Complete" here means that every level must match in sequence. For
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
#' @param type type of join ("left", "inner", "anti") (defaults to "left")
#' @param ref_prefix Prefix to add to hierarchical column names in `ref` if they
#'   are otherwise identical to names in `raw`  (defaults to `ref_`)
#' @param std_fn Function to standardize strings during matching. Defaults to
#'   \code{\link{string_std}}. Set to `NULL` to omit standardization. See
#'   also \link{string_standardization}.
#'
#' @return A `data.frame` obtained by matching the hierarchical columns in `raw`
#'   and `ref`. If `type == "inner"`, returns only the rows of `raw` with a
#'   single match in `ref`. If `type == "left"`, returns all rows of `raw`.
#'
#' @examples
#' data(ne_raw)
#' data(ne_ref)
#'
#' hmatch_complete(ne_raw, ne_ref, pattern_raw = "adm", type = "inner")
#'
#' @export hmatch_complete
hmatch_complete <- function(raw,
                            ref,
                            pattern_raw = NULL,
                            pattern_ref = pattern_raw,
                            by = NULL,
                            type = "left",
                            ref_prefix = "ref_",
                            std_fn = string_std) {

  # # for testing
  # raw = ne_raw
  # ref = ne_ref
  # pattern_raw = NULL
  # pattern_ref = pattern_raw
  # by = NULL
  # type = "left"
  # ref_prefix = "ref_"
  # std_fn = string_std

  if (!is.null(std_fn)) std_fn <- match.fun(std_fn)

  ## identify hierarchical columns to match, and rename ref cols if necessary
  prep <- prep_match_columns(raw = raw,
                             ref = ref,
                             pattern_raw = pattern_raw,
                             pattern_ref = pattern_ref,
                             by = by,
                             ref_prefix = ref_prefix)

  ## add standardized columns for joining
  raw_join <- add_join_columns(dat = raw,
                               by = prep$by_raw,
                               join_cols = prep$by_join,
                               std_fn = std_fn)

  ref_join <- add_join_columns(dat = prep$ref,
                               by = prep$by_ref,
                               join_cols = prep$by_join,
                               std_fn = std_fn)

  ref_join$TEMP_IS_MATCH <- "MATCH"

  ## merge raw and ref
  out <- merge(raw_join,
               ref_join,
               by = prep$by_join,
               all.x = TRUE)

  ## execute merge type
  out <- switch(type,
                "inner" = out[!is.na(out$TEMP_IS_MATCH),],
                "anti" = out[is.na(out$TEMP_IS_MATCH),],
                out)

  ## remove extra columns and return
  out[,!names(out) %in% c(prep$by_join, "TEMP_IS_MATCH"), drop = FALSE]
}

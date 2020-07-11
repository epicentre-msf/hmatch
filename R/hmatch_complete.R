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
#' @param pattern regex pattern to match the hierarchical columns in `raw`
#'   (columns can be matched using either the `pattern` or `by` arguments, as
#'   described in \link{specifying_columns})
#' @param pattern_ref regex pattern to match the hierarchical columns in `ref`
#'   (defaults to `pattern`)
#' @param by vector giving the names of the hierarchical columns in `raw`
#' @param by_ref vector giving the names of the hierarchical columns in `ref`
#'   (defaults to `by`)
#' @param type type of join ("left", "inner", "inner_unique", "anti", or
#'   "anti_unique"). Defaults to "left". See \link{join_types}.
#' @param dict Optional dictionary for recoding values within the hierarchical
#'   columns of `raw` (see \link{dictionary_recoding})
#' @param ref_prefix Prefix to add to hierarchical column names in `ref` if they
#'   are otherwise identical to names in `raw`  (defaults to `ref_`)
#' @param std_fn Function to standardize strings during matching. Defaults to
#'   \code{\link{string_std}}. Set to `NULL` to omit standardization. See
#'   also \link{string_standardization}.
#' @param ... Additional arguments passed to `std_fn()`
#'
#' @return a data frame obtained by matching the hierarchical columns in `raw`
#'   and `ref`, using the join type specified by argument `type` (see
#'   \link{join_types} for more details)
#'
#' @examples
#' data(ne_raw)
#' data(ne_ref)
#'
#' hmatch_complete(ne_raw, ne_ref, pattern = "^adm")
#'
#' # with dictionary-based recoding
#' ne_dict <- data.frame(value = "USA",
#'                       replacement = "United States",
#'                       variable = "adm0")
#'
#' hmatch_complete(ne_raw, ne_ref, pattern = "^adm", dict = ne_dict)
#'
#' @export hmatch_complete
hmatch_complete <- function(raw,
                            ref,
                            pattern = NULL,
                            pattern_ref = pattern,
                            by = NULL,
                            by_ref = by,
                            type = "left",
                            dict = NULL,
                            ref_prefix = "ref_",
                            std_fn = string_std,
                            ...) {

  # # for testing
  # raw = ne_raw
  # ref = ne_ref
  # # raw$adm2[1] <- "Suffolk II"
  # # ref$adm2[10] <- "Suffolk 2"
  # pattern = NULL
  # pattern_ref = pattern
  # by = NULL
  # by_ref = by
  # dict <- ne_dict
  # type = "inner"
  # ref_prefix = "ref_"
  # std_fn = string_std
  # ... <- NULL

  ## match args
  if (!is.null(std_fn)) std_fn <- match.fun(std_fn)
  type <- match.arg(type, c("left", "inner", "inner_unique", "anti", "anti_unique"))

  ## identify hierarchical columns to match, and rename ref cols if necessary
  prep <- prep_match_columns(
    raw = raw,
    ref = ref,
    pattern = pattern,
    pattern_ref = pattern_ref,
    by = by,
    by_ref = by_ref,
    ref_prefix = ref_prefix
  )

  ## add standardized columns for joining
  raw_join <- add_join_columns(
    dat = raw,
    by = prep$by_raw,
    join_cols = prep$by_raw_join,
    std_fn = std_fn,
    ...
  )

  ref_join <- add_join_columns(
    dat = prep$ref,
    by = prep$by_ref,
    join_cols = prep$by_raw_join,
    std_fn = std_fn,
    ...
  )

  ### implement dictionary recoding on join columns
  if (!is.null(dict)) {
    raw_join <- apply_dict(
      raw_join,
      dict,
      by_raw = prep$by_raw,
      by_join = prep$by_raw_join,
      std_fn = std_fn
    )
  }

  ## run main matching routine
  hmatch_complete_(
    raw_join,
    ref_join,
    by_raw_join = prep$by_raw_join,
    type = type,
    class_raw = class(raw)
  )
}




#' @noRd
#' @importFrom dplyr left_join
hmatch_complete_ <- function(raw_join,
                             ref_join,
                             by_raw_join,
                             by_ref_join = by_raw_join,
                             type = "left",
                             class_raw = "data.frame") {


  ## add temporary row-id column to aid in matching
  temp_col_id <- "TEMP_ROW_ID_COMPLETE"
  raw_join[[temp_col_id]] <- seq_len(nrow(raw_join))

  ## add temporary match column to ref_join
  temp_col_match <- "TEMP_MATCH_COMPLETE"
  ref_join[[temp_col_match]] <- TRUE

  ## re-derive initial (pre-join) column names
  names_raw_prep <- setdiff(names(raw_join), by_raw_join)
  names_raw_orig <- setdiff(names_raw_prep, temp_col_id)

  ## complete join
  matches_out <- dplyr::left_join(
    raw_join,
    ref_join,
    by = set_names(by_ref_join, by_raw_join)
  )

  ## remove join cols
  matches_out <- matches_out[, !names(matches_out) %in% by_raw_join, drop = FALSE]

  ## execute match type and remove temporary columns
  prep_output(
    x = matches_out,
    type = type,
    temp_col_id = temp_col_id,
    temp_col_match = temp_col_match,
    cols_raw_orig = names_raw_orig,
    class_raw = class_raw
  )
}

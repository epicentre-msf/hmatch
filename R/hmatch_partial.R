#' Partial hierarchical matching
#'
#' Match a data.frame with raw, potentially messy hierarchical data (e.g.
#' province, county, township) against a reference dataset, using partial
#' matching. "Partial" here means that one or more hierarchical levels within
#' the raw data may be missing (i.e. NA). More specifically, for a given row of
#' raw data, matches can potentially be made to a high-resolution level (e.g.
#' township) even if one or more lower-resolution levels (e.g. province) is
#' missing.
#'
#' @param raw `data.frame` containing hierarchical columns with raw, potentially
#'   messy data
#' @param ref `data.frame` containing hierarchical columns with reference data
#' @param pattern_raw regex pattern to match the hierarchical columns in `raw`
#'   (see also \link{specifying_columns})
#' @param pattern_ref regex pattern to match the hierarchical columns in `ref`
#'   (see also \link{specifying_columns})
#' @param by named character vector whose elements are the names of the
#'   hierarchical columns in `ref` and whose names are the names of the
#'   corresponding columns in `raw` (see also \link{specifying_columns})
#' @param dict Optional dictionary for recoding values within the hierarchical
#'   columns of `raw` (see \link{dictionary_recoding})
#' @param type type of join ("left", "inner", "inner_unique", "anti", or
#'   "anti_unique"). Defaults to "left". See \link{join_types}.
#' @param ref_prefix Prefix to add to hierarchical column names in `ref` if they
#'   are otherwise identical to names in `raw`  (defaults to `ref_`)
#' @param fuzzy logical indicating whether to use fuzzy-matching (defaults to
#'   FALSE)
#' @param max_dist if `fuzzy = TRUE`, the maximum string distance to use when
#'   fuzzy-matching (defaults to `1L`)
#' @param std_fn Function to standardize strings during matching. Defaults to
#'   \code{\link{string_std}}. Set to `NULL` to omit standardization. See
#'   also \link{string_standardization}.
#' @param ... Additional arguments passed to `std_fn()`
#'
#' @return A `data.frame` obtained by matching the hierarchical columns in `raw`
#'   and `ref`. If `type == "inner"`, returns only the rows of `raw` with a
#'   single match in `ref`. If `type == "left"`, returns all rows of `raw`.
#'
#' @examples
#' data(ne_raw)
#' data(ne_ref)
#'
#' hmatch_partial(ne_raw, ne_ref, pattern_raw = "adm")
#'
#' # with dictionary-based recoding
#' ne_dict <- data.frame(value = "USA",
#'                       replacement = "United States",
#'                       variable = "adm0")
#'
#' hmatch_partial(ne_raw, ne_ref, pattern_raw = "adm", dict = ne_dict)
#'
#' @importFrom dplyr left_join
#' @export hmatch_partial
hmatch_partial <- function(raw,
                           ref,
                           pattern_raw = NULL,
                           pattern_ref = pattern_raw,
                           by = NULL,
                           dict = NULL,
                           type = "left",
                           ref_prefix = "ref_",
                           fuzzy = FALSE,
                           max_dist = 1L,
                           std_fn = string_std,
                           ...) {

  # # for testing
  # raw = ne_raw
  # ref = ne_ref
  # raw$adm2[1] <- "Suffolk II"
  # ref$adm2[10] <- "Suffolk 2"
  # pattern_raw = NULL
  # pattern_ref = pattern_raw
  # by = NULL
  # dict <- NULL
  # type = "left"
  # ref_prefix = "ref_"
  # fuzzy = TRUE
  # max_dist = 1L
  # std_fn = string_std
  # ... <- NULL

  if (!is.null(std_fn)) std_fn <- match.fun(std_fn)
  type <- match.arg(type, c("left", "inner", "inner_unique", "anti", "anti_unique"))

  ## identify hierarchical columns to match, and rename ref cols if necessary
  prep <- prep_match_columns(raw = raw,
                             ref = ref,
                             pattern_raw = pattern_raw,
                             pattern_ref = pattern_ref,
                             by = by,
                             ref_prefix = ref_prefix)

  ## save original column names of raw
  raw_cols_orig <- names(raw)

  ## create temporary row id in raw
  temp_id_col <- "TEMP_ROW_ID_PART"
  raw[[temp_id_col]] <- seq_len(nrow(raw))

  ## join colnames must be separate from original, and differ between raw/ref
  by_raw_join <- paste0(prep$by_raw, "___JOIN_")
  by_ref_join <- paste0(prep$by_ref, "___JOIN_")

  ## add standardized columns for joining
  raw_join <- add_join_columns(dat = raw,
                               by = prep$by_raw,
                               join_cols = by_raw_join,
                               std_fn = std_fn,
                               ...)

  ref_join <- add_join_columns(dat = prep$ref,
                               by = prep$by_ref,
                               join_cols = by_ref_join,
                               std_fn = std_fn,
                               ...)

  ### implement dictionary recoding on join columns
  if (!is.null(dict)) {
    raw_join <- apply_dict(raw_join,
                           dict,
                           by_raw = prep$by_raw,
                           by_join = prep$by_join,
                           std_fn = std_fn)
  }

  ## add max non-missing adm level
  raw_join[["MAX_ADM_RAW_"]] <- max_adm_level(raw_join, by = prep$by_raw)
  ref_join[["MAX_ADM_REF_"]] <- max_adm_level(ref_join, by = prep$by_ref)

  ## extract only the join columns
  raw_ <- raw_join[,by_raw_join, drop = FALSE]
  ref_ <- ref_join[,by_ref_join, drop = FALSE]

  ## identify the maximum (highest-resolution) hierarchical level
  max_level <- length(prep$by_raw)

  col_max_raw <- by_raw_join[max_level]
  col_max_ref <- by_ref_join[max_level]

  ## generate all possible match combinations at max hierarchical level
  initial_combinations <- expand.grid(x = unique(raw_[[col_max_raw]]),
                                      y = unique(ref_[[col_max_ref]]),
                                      stringsAsFactors = FALSE)

  names(initial_combinations) <- c(col_max_raw, col_max_ref)

  ## filter to actual matches at max hierarchical level
  initial_matches <- filter_to_matches(dat = initial_combinations,
                                       col1 = col_max_raw,
                                       col2 = col_max_ref,
                                       fuzzy = fuzzy,
                                       max_dist = max_dist,
                                       is_max_level = TRUE)

  ## rejoin raw and ref
  initial_matches_join <- dplyr::left_join(initial_matches,
                                           raw_join,
                                           by = col_max_raw)

  initial_matches_join <- dplyr::left_join(initial_matches_join,
                                           ref_join,
                                           by = col_max_ref)

  ## filter to matches where the max ref level is <= the max raw level
  i <- initial_matches_join[["MAX_ADM_REF_"]] <= initial_matches_join[["MAX_ADM_RAW_"]]
  matches_remaining <- initial_matches_join[i,]

  ## for each lower hierarchical level...
  if (max_level > 1) {
    for (j in (max_level - 1):1) {
      col_focal_raw <- names(raw_)[j]
      col_focal_ref <- names(ref_)[j]

      matches_remaining <- filter_to_matches(dat = matches_remaining,
                                             col1 = col_focal_raw,
                                             col2 = col_focal_ref,
                                             fuzzy = fuzzy,
                                             max_dist = max_dist,
                                             is_max_level = FALSE)
    }
  }

  ## remove join columns and filter to unique rows
  matches_out <- unique(matches_remaining[,c(raw_cols_orig, names(prep$ref))])

  ## add temporary column to track matches after merge
  matches_out <- add_column(matches_out, "TEMP_IS_MATCH", "MATCH")

  ## merge final match data with raw
  out <- dplyr::left_join(raw, matches_out, by = raw_cols_orig)

  ## execute merge type
  dup_ids <- out[[temp_id_col]][duplicated(out[[temp_id_col]])]

  if (type == "inner") {
    out <- out[!is.na(out$TEMP_IS_MATCH),]
  } else if (type == "inner_unique") {
    rows_keep <- !is.na(out$TEMP_IS_MATCH) & !out[[temp_id_col]] %in% dup_ids
    out <- out[rows_keep,]
  } else if (type == "anti") {
    out <- out[is.na(out$TEMP_IS_MATCH), c(names(raw), "TEMP_IS_MATCH")]
  } else if (type == "anti_unique") {
    rows_keep <- is.na(out$TEMP_IS_MATCH) | out[[temp_id_col]] %in% dup_ids
    out <- unique(out[rows_keep, c(names(raw), "TEMP_IS_MATCH")])
  }

  ## arrange rows and reclass out to match raw (tibble classes with otherwise be
  # stripped)
  out <- out[order(out[[temp_id_col]]),]
  class(out) <- class(raw)

  ## remove temporary columns and return
  out[,!names(out) %in% c(temp_id_col, "TEMP_IS_MATCH"), drop = FALSE]
}



#' @noRd
#' @importFrom stringdist stringdist
filter_to_matches <- function(dat, col1, col2, fuzzy, max_dist, is_max_level) {

  match <- if (fuzzy) {
    stringdist::stringdist(dat[[col1]], dat[[col2]]) <= max_dist
  } else {
    dat[[col1]] == dat[[col2]]
  }

  keep <- if (is_max_level) {
    match | is.na(dat[[col1]]) & is.na(dat[[col2]])
  } else {
    match | is.na(dat[[col1]])
  }

  keep[is.na(keep)] <- FALSE

  return(dat[keep,])
}


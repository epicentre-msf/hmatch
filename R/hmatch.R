#' Match sets of hierarchical variables between a raw and reference dataset
#'
#' @description
#' Match sets of hierarchical values (e.g. province, county, township) in a raw,
#' messy dataset to corresponding values within a reference dataset, optionally
#' accounting for discrepancies between the datasets such as:
#'
#' - variation in character case, use of accents, or spelling
#' - variation in hierarchical resolution (e.g. some entries specified to
#'   municipality but others only to region)
#' - missing values at one or more hierarchical levels
#'
#' @param raw data frame containing hierarchical columns with raw data
#' @param ref data frame containing hierarchical columns with reference data
#' @param pattern regex pattern to match the hierarchical columns in `raw`\cr
#'
#' **Note:** hierarchical column names can be matched using either the `pattern`
#' *or* `by` arguments. Or, if neither `pattern` or `by` are specified, the
#' hierarchical columns are assumed to be all column names that are common to
#' both `raw` and `ref`. See \link{specifying_columns}.
#' @param pattern_ref regex pattern to match the hierarchical columns in `ref`.
#'   Defaults to `pattern`, so only need to specify if the hierarchical columns
#'   have different names in `raw` and `ref`.
#' @param by vector giving the names of the hierarchical columns in `raw`
#' @param by_ref vector giving the names of the hierarchical columns in `ref`.
#'   Defaults to `by`, so only need to specify if the hierarchical columns
#'   have different names in `raw` and `ref`.
#' @param type type of join ("left", "inner", "anti", "resolve_left",
#'   "resolve_inner", or "resolve_anti"). Defaults to "left". See
#'   \link{join_types}.
#' @param allow_gaps logical indicating whether to allow missing values below
#'   the match level, where 'match level' is the highest level with a
#'   non-missing value within a given row of `raw`. Defaults to `TRUE`.
#' @param fuzzy logical indicating whether to use fuzzy-matching (based on the
#'   \code{\link{stringdist}} package). Defaults to FALSE.
#' @param fuzzy_method if `fuzzy = TRUE`, the method to use for string distance
#'   calculation (see \link[stringdist]{stringdist-metrics}). Defaults to "osa".
#' @param fuzzy_dist if `fuzzy = TRUE`, the maximum string distance to use to
#'   classify matches (i.e. a string distance ≤ `fuzzy_dist` will be considered
#'   matching). Defaults to `1L`.
#' @param dict optional dictionary for recoding values within the hierarchical
#'   columns of `raw` (see \link{dictionary_recoding})
#' @param ref_prefix prefix to add to names of returned columns from `ref` if
#'   they are otherwise identical to names within `raw`. Defaults to "ref_".
#' @param std_fn function to standardize strings during matching. Defaults to
#'   \code{\link{string_std}}. Set to `NULL` to omit standardization. See
#'   also \link{string_standardization}.
#' @param ... additional arguments passed to `std_fn()`
#'
#' @return a data frame obtained by matching the hierarchical columns in `raw`
#'   and `ref`, using the join type specified by argument `type` (see
#'   \link{join_types} for more details)
#'
#' @section Resolve joins:
#' In `hmatch`, if argument `type` corresponds to a resolve join, rows
#' of `raw` with multiple matches to `ref` are always resolved to 'no match'.
#' This is because `hmatch` does not accept matches below the highest
#' non-missing level within a given row of `raw`. E.g.
#'
#' `raw`: \cr
#' `1. | United States | <NA>         | Jefferson |` \cr
#'
#' Relevant rows from `ref`: \cr
#' `1. | United States | New York     | Jefferson |` \cr
#' `2. | United States | Pennsylvania | Jefferson |`
#'
#' In a regular join with `hmatch`, the single row from `raw` (above)
#' will match both rows of `ref`. However, in a resolve join the multiple
#' conflicting matches (i.e. conflicting values at the 2nd hierarchical level)
#' will result in the row from `raw` being treated as non-matching to `ref`.
#'
#' @examples
#' data(ne_raw)
#' data(ne_ref)
#'
#' hmatch(ne_raw, ne_ref, pattern = "adm", type = "inner")
#'
#' # with dictionary-based recoding
#' ne_dict <- data.frame(value = "USA",
#'                       replacement = "United States",
#'                       variable = "adm0")
#'
#' hmatch(ne_raw, ne_ref, pattern = "adm", dict = ne_dict)
#'
#' @export hmatch
hmatch <- function(raw,
                   ref,
                   pattern,
                   pattern_ref = pattern,
                   by,
                   by_ref = by,
                   type = "left",
                   allow_gaps = TRUE,
                   fuzzy = FALSE,
                   fuzzy_method = "osa",
                   fuzzy_dist = 1L,
                   dict = NULL,
                   ref_prefix = "ref_",
                   std_fn = string_std,
                   ...) {


  # # for testing purposed only
  # raw = ne_raw
  # ref = ne_ref
  # raw$adm2[1] <- "Suffolk II"
  # ref$adm2[10] <- "Suffolk 2"
  # pattern = NULL
  # pattern_ref = pattern
  # by = NULL
  # by_ref = by
  # allow_gaps = TRUE
  # dict <- NULL
  # type = "inner"
  # ref_prefix = "ref_"
  # fuzzy = TRUE
  # fuzzy_method = "osa"
  # fuzzy_dist = 1L
  # std_fn = string_std
  # ... <- NULL

  ## match args
  if (!is.null(std_fn)) std_fn <- match.fun(std_fn)
  type <- match.arg(type, c("left", "inner", "anti", "resolve_left", "resolve_inner", "resolve_anti"))

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
    join_cols = prep$by_ref_join,
    std_fn = std_fn,
    ...
  )

  ## implement dictionary recoding on join columns
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
  hmatch_(
    raw_join = raw_join,
    ref_join = ref_join,
    by_raw = prep$by_raw,
    by_ref = prep$by_ref,
    by_raw_join = prep$by_raw_join,
    by_ref_join = prep$by_ref_join,
    allow_gaps = allow_gaps,
    type = type,
    fuzzy = fuzzy,
    fuzzy_dist = fuzzy_dist,
    class_raw = class(raw)
  )
}



#' Wrapper for lower level matching functions hmatch__ and hmatch_complete__
#'
#' Used because hmatch_complete__ (doesn't allow for gaps or fuzzy matching) is
#' much faster than hmatch__, so only use hmatch__ for fuzzy matching and/or
#' matching rows of raw with gaps
#'
#' @noRd
#' @importFrom dplyr bind_rows
hmatch_ <- function(raw_join,
                    ref_join,
                    by_raw = NULL,
                    by_ref = NULL,
                    by_raw_join,
                    by_ref_join,
                    type = "left",
                    allow_gaps = TRUE,
                    fuzzy = FALSE,
                    fuzzy_method = "osa",
                    fuzzy_dist = 1L,
                    class_raw = "data.frame") {


  ## temp row id
  temp_col_id <- "TEMP_ROW_ID_PART_WRAPPER"
  raw_join[[temp_col_id]] <- seq_len(nrow(raw_join))

  if (!fuzzy & !allow_gaps) {
    ## if not fuzzy and gaps not allowed, use complete match for all rows

    out <- hmatch_complete__(
      raw_join,
      ref_join,
      by_raw = by_raw,
      by_ref = by_ref,
      by_raw_join = by_raw_join,
      by_ref_join = by_ref_join,
      type = type,
      class_raw = class_raw
    )

  } else if (!fuzzy & allow_gaps) {
    ## else if not fuzzy and allow gaps, use complete match for complete rows
    # and partial for rest

    no_gaps_raw_join <- complete_sequence(raw_join, by = by_raw_join)
    raw_join_complete <- raw_join[no_gaps_raw_join, , drop = FALSE]
    raw_join_partial <- raw_join[!no_gaps_raw_join, , drop = FALSE]

    out_complete <- hmatch_complete__(
      raw_join_complete,
      ref_join,
      by_raw = by_raw,
      by_ref = by_ref,
      by_raw_join = by_raw_join,
      by_ref_join = by_ref_join,
      type = type,
      class_raw = class_raw
    )

    out_partial <- hmatch__(
      raw_join_partial,
      ref_join,
      by_raw = by_raw,
      by_ref = by_ref,
      by_raw_join = by_raw_join,
      by_ref_join = by_ref_join,
      allow_gaps = allow_gaps,    # always TRUE in this block
      fuzzy = fuzzy,              # always FALSE in this block
      fuzzy_method = fuzzy_method,
      fuzzy_dist = fuzzy_dist,
      type = type,
      class_raw = class_raw
    )

    out <- dplyr::bind_rows(out_complete, out_partial)

  } else {
    ## else if fuzzy, use partial match for all rows

    out <- hmatch__(
      raw_join = raw_join,
      ref_join = ref_join,
      by_raw = by_raw,
      by_ref = by_ref,
      by_raw_join = by_raw_join,
      by_ref_join = by_ref_join,
      allow_gaps = allow_gaps,
      type = type,
      fuzzy = fuzzy,     # always TRUE in this block
      fuzzy_method = fuzzy_method,
      fuzzy_dist = fuzzy_dist,
      class_raw = class_raw
    )
  }

  ## reorder rows, and remove temp column and rownames
  out <- out[order(out[[temp_col_id]]),]
  row.names(out) <- NULL
  out[,!names(out) %in% temp_col_id, drop = FALSE]
}



#' Low level matching function that allows for gaps and fuzzy matching
#' @noRd
#' @importFrom dplyr left_join
hmatch__ <- function(raw_join,
                     ref_join,
                     by_raw = NULL, # not used
                     by_ref = NULL, # only used if type is resolve join
                     by_raw_join,
                     by_ref_join,
                     allow_gaps = TRUE,
                     type = "left",
                     fuzzy = FALSE,
                     fuzzy_method = "osa",
                     fuzzy_dist = 1L,
                     class_raw = "data.frame") {


  ## add temporary row-id column to aid in matching
  temp_col_id <- "TEMP_ROW_ID_PART"
  raw_join[[temp_col_id]] <- seq_len(nrow(raw_join))

  ## add temporary match column to ref_join
  temp_col_match <- "TEMP_MATCH_PART"
  ref_join[[temp_col_match]] <- rep(TRUE, nrow(ref_join))

  ## re-derive initial (pre-join) column names
  names_raw_prep <- setdiff(names(raw_join), by_raw_join)
  names_raw_orig <- setdiff(names_raw_prep, temp_col_id)
  names_ref_prep <- setdiff(names(ref_join), by_ref_join)

  ## add max non-missing adm level
  temp_col_max_raw <- "MAX_ADM_RAW_"
  temp_col_max_ref <- "MAX_ADM_REF_"
  raw_join[[temp_col_max_raw]] <- max_levels(raw_join, by = by_raw_join)
  ref_join[[temp_col_max_ref]] <- max_levels(ref_join, by = by_ref_join)

  ## if !allow_gaps, filter now to complete sequences for efficiency
  raw_join_orig <- raw_join

  if (!allow_gaps) {
    rows_no_gaps <- complete_sequence(raw_join, by_raw_join)
    raw_join <- raw_join[rows_no_gaps, , drop = FALSE]
  }

  ## extract only the join columns
  raw_ <- raw_join[,by_raw_join, drop = FALSE]
  ref_ <- ref_join[,by_ref_join, drop = FALSE]

  ## identify the min and maximum hierarchical levels
  max_level <- length(by_raw_join)

  col_max_raw <- by_raw_join[max_level]
  col_max_ref <- by_ref_join[max_level]

  col_min_raw <- by_raw_join[1]
  col_min_ref <- by_ref_join[1]

  ## raw/ref combinations at first hierarchical level
  initial_combinations <- expand.grid(
    x = unique(raw_[[col_min_raw]]),
    y = unique(ref_[[col_min_ref]]),
    stringsAsFactors = FALSE
  )

  names(initial_combinations) <- c(col_min_raw, col_min_ref)

  ## filter to actual matches at max hierarchical level
  matches_remaining <- filter_to_matches(
    x = initial_combinations,
    col1 = col_min_raw,
    col2 = col_min_ref,
    fuzzy = fuzzy,
    fuzzy_method = fuzzy_method,
    fuzzy_dist = fuzzy_dist,
    is_max_level = FALSE
  )

  ## for each lower hierarchical level...
  if (max_level > 1) {
    for (j in 2:max_level) {

      ## identify relevant columns
      col_focal_raw <- by_raw_join[j]
      col_focal_ref <- by_ref_join[j]

      cols_prev_raw <- by_raw_join[1:(j - 1)]
      cols_prev_ref <- by_ref_join[1:(j - 1)]

      col_up_to_focal_raw <- by_raw_join[1:j]
      col_up_to_focal_ref <- by_ref_join[1:j]

      ## prepare dfs for joining next hierarchical level in raw and ref
      next_join_raw <- unique(raw_[,col_up_to_focal_raw, drop = FALSE])
      next_join_ref <- unique(ref_[,col_up_to_focal_ref, drop = FALSE])

      ## join next levels of raw and ref
      matches_remaining <- dplyr::inner_join(
        matches_remaining,
        next_join_raw,
        by = cols_prev_raw
      )

      matches_remaining <- dplyr::inner_join(
        matches_remaining,
        next_join_ref,
        by = cols_prev_ref
      )

      ## filter to matches at current hierarchical level
      matches_remaining <- filter_to_matches(
        x = matches_remaining,
        col1 = col_focal_raw,
        col2 = col_focal_ref,
        fuzzy = fuzzy,
        fuzzy_method = fuzzy_method,
        fuzzy_dist = fuzzy_dist,
        is_max_level = col_focal_raw == col_max_raw
      )
    }
  }

  ## match bare join columns back to raw_join and ref_join
  matches_join_out <- dplyr::inner_join(
    raw_join[, c(temp_col_id, temp_col_max_raw, by_raw_join)],
    matches_remaining,
    by = by_raw_join
  )

  matches_join_out <- dplyr::inner_join(
    matches_join_out,
    ref_join,
    by = by_ref_join
  )

  ## filter to matches where the max ref level is <= the max raw level
  keep <- matches_join_out[[temp_col_max_ref]] <= matches_join_out[[temp_col_max_raw]]
  matches_join_out <- matches_join_out[keep, , drop = FALSE]

  ## remove join columns and filter to unique rows
  matches_join_out <- unique(matches_join_out[,c(temp_col_id, names_ref_prep)])

  ## if resolve-type join
  if (grepl("^resolve", type)) {
    matches_join_out <- resolve_join(
      matches_join_out,
      by_ref = by_ref,
      temp_col_id = temp_col_id,
      consistent = "all"
    )
  }

  ## merge raw with final match data
  raw_join_out <- raw_join_orig[,names_raw_prep, drop = FALSE]
  matches_out <- dplyr::left_join(raw_join_out, matches_join_out, by = temp_col_id)

  ## execute match type and remove temporary columns
  prep_output(
    x = matches_out,
    type = gsub("^resolve_", "", type),
    temp_col_id = temp_col_id,
    temp_col_match = temp_col_match,
    cols_raw_orig = names_raw_orig,
    class_raw = class_raw
  )
}



#' @noRd
#' @importFrom stringdist stringdist
filter_to_matches <- function(x,
                              col1,
                              col2,
                              fuzzy,
                              fuzzy_method,
                              fuzzy_dist,
                              is_max_level) {

  match <- if (fuzzy) {
    stringdist::stringdist(x[[col1]], x[[col2]], method = fuzzy_method) <= fuzzy_dist
  } else {
    x[[col1]] == x[[col2]]
  }

  keep <- if (is_max_level) {
    match | is.na(x[[col1]]) & is.na(x[[col2]])
  } else {
    match | is.na(x[[col1]])
  }

  ## convert NA to FALSE and return matching rows of dat
  keep[is.na(keep)] <- FALSE
  x[keep, , drop = FALSE]
}




#' Low level matching function that doesn't allow for gaps or fuzzy matching
#' @noRd
#' @importFrom dplyr left_join
hmatch_complete__ <- function(raw_join,
                              ref_join,
                              by_raw = NULL, # not used
                              by_ref = NULL, # only used if type is resolve join
                              by_raw_join,
                              by_ref_join = by_raw_join,
                              type = "left",
                              class_raw = "data.frame") {

  ## add temporary row-id column to aid in matching
  temp_col_id <- "TEMP_ROW_ID_COMPLETE"
  raw_join[[temp_col_id]] <- seq_len(nrow(raw_join))

  ## add temporary match column to ref_join
  temp_col_match <- "TEMP_MATCH_COMPLETE"
  ref_join[[temp_col_match]] <- rep(TRUE, nrow(ref_join))

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

  ## if resolve-type join
  if (grepl("^resolve", type)) {
    matches_out <- resolve_join(
      matches_out,
      by_ref = by_ref,
      temp_col_id = temp_col_id,
      consistent = "all"
    )
  }

  ## execute match type and remove temporary columns
  prep_output(
    x = matches_out,
    type = gsub("^resolve_", "", type),
    temp_col_id = temp_col_id,
    temp_col_match = temp_col_match,
    cols_raw_orig = names_raw_orig,
    class_raw = class_raw
  )
}


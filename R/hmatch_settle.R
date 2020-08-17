#' Sequential hierarchical matching at each hierarchical level, settling for the
#' highest resolution match that is possible for each row
#'
#' @description
#' Match sets of hierarchical values (e.g. province / county / township) in a
#' raw, messy dataset to corresponding values within a reference dataset,
#' sequentially over each hierarchical level. Specifically, implements
#' \code{\link{hmatch}} at each successive hierarchical level, starting with
#' only the first level (lowest resolution), then first and second, first second
#' and third, etc.
#'
#' After the initial matching over all levels, users can optionally use a
#' resolve join to 'settle' for the highest match possible for each row of raw
#' data, even if that match is below the highest-resolution level specified.
#'
#' @inheritParams hmatch
#' @inherit hmatch return
#'
#' @section Resolve joins:
#' In a resolve type join with `hmatch_settle`, rows of `raw` with multiple
#' matches to `ref` are resolved to the highest hierarchical level that is
#' non-conflicting among all matches (or no match if there is a conflict at the
#' very first level). E.g.
#'
#' `raw`: \cr
#' `1. | United States | <NA>         | Jefferson |` \cr
#'
#' Relevant rows from `ref`: \cr
#' `1. | United States | <NA>         | <NA>      |` \cr
#' `2. | United States | New York     | Jefferson |` \cr
#' `3. | United States | Pennsylvania | Jefferson |`
#'
#' In a regular join, the single row from `raw` (above) will match all three
#' rows from `ref`. However, in a resolve join the multiple matches will be
#' resolved to the first row from `ref`, because only the first hierarchical
#' level ("United States") is non-conflicting among all possible matches.
#'
#' Note that there's a distinction between "common" values at a given
#' hierarchical level (i.e. a single unique value in each row) and
#' "non-conflicting" values (i.e. a single unique value *or* a missing value).
#' E.g.
#'
#' `raw`: \cr
#' `1. | United States | New York | New York |` \cr
#'
#' Relevant rows from `ref`: \cr
#' `1. | United States | <NA>     | <NA>     |` \cr
#' `2. | United States | New York | <NA>     |` \cr
#' `3. | United States | New York | New York |`
#'
#' In the example above, only the 1st hierarchical level ("United States") is
#' "common" to all matches, but all hierarchical levels are "non-conflicting"
#' (i.e. because row 2 is a hierarchical child of row 1, and row 3 a child of
#' row 2), and so a resolve-type match will be made to the 3rd row in `ref`.
#'
#' @examples
#' data(ne_raw)
#' data(ne_ref)
#'
#' # return matches at all levels
#' hmatch_settle(ne_raw, ne_ref, pattern = "^adm", type = "inner")
#'
#' # use a resolve join to settle for the best possible match for each row
#' hmatch_settle(ne_raw, ne_ref, pattern = "^adm", type = "resolve_inner")
#'
#' @export hmatch_settle
hmatch_settle <- function(raw,
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

  ## run main matching routines
  hmatch_settle_(
    raw_join = raw_join,
    ref_join = ref_join,
    by_raw = prep$by_raw,
    by_ref = prep$by_ref,
    by_raw_join = prep$by_raw_join,
    by_ref_join = prep$by_ref_join,
    type = type,
    allow_gaps = allow_gaps,
    fuzzy = fuzzy,
    fuzzy_method = fuzzy_method,
    fuzzy_dist = fuzzy_dist,
    class_raw = class(raw)
  )
}


#' @noRd
#' @importFrom dplyr left_join
hmatch_settle_ <- function(raw_join,
                           ref_join,
                           by_raw,
                           by_ref,
                           by_raw_join,
                           by_ref_join,
                           type = "left",
                           allow_gaps = TRUE,
                           fuzzy = FALSE,
                           fuzzy_method = "osa",
                           fuzzy_dist = 1L,
                           class_raw = "data.frame") {

  ## temporary columns to aid in matching
  temp_col_match <- "TEMP_COL_MATCH_SETTLE"
  temp_col_id <- "TEMP_ROW_ID_SETTLE"
  raw_join[[temp_col_id]] <- seq_len(nrow(raw_join))

  ## re-derive initial column names
  names_raw_prep <- setdiff(names(raw_join), by_raw_join)
  names_raw_orig <- setdiff(names_raw_prep, temp_col_id)
  names_ref_prep <- setdiff(names(ref_join), by_ref_join)

  ## temp column to track max level of ref
  temp_col_max_level <- "TEMP_MAX_LEVEL_SETTLE"
  ref_join[[temp_col_max_level]] <- max_levels(ref_join, by = by_ref)

  matches_by_level <- list()

  ## find matches with hmatch() at each match level...
  for (j in rev(seq_along(by_raw))) {

    # columns to exclude (> focal level)
    j_excl <- setdiff(seq_along(by_raw), seq_len(j))

    cols_excl_raw <- c(by_raw[j_excl], by_raw_join[j_excl])
    cols_excl_ref <- c(by_ref[j_excl], by_ref_join[j_excl])

    # subset to focal columns of raw and ref
    raw_foc <- raw_join[,!names(raw_join) %in% cols_excl_raw, drop = FALSE]
    ref_foc <- ref_join[,!names(ref_join) %in% cols_excl_ref, drop = FALSE]

    # filter ref to rows where max_level is <= the focal level
    ref_foc <- ref_foc[ref_foc[[temp_col_max_level]] <= j,]

    # match raw to ref at given level
    matches_by_level[[j]] <-  hmatch_(
      raw_join = raw_foc,
      ref_join = ref_foc,
      by_raw_join = by_raw_join[1:j],
      by_ref_join = by_ref_join[1:j],
      type = "inner",
      allow_gaps = allow_gaps,
      fuzzy = fuzzy,
      fuzzy_method = fuzzy_method,
      fuzzy_dist = fuzzy_dist,
      class_raw = class_raw
    )
  }

  ## prepare match data for join
  matches_prep <- dplyr::bind_rows(matches_by_level)
  matches_join_out <- unique(matches_prep[,c(temp_col_id, names_ref_prep), drop = FALSE])
  matches_join_out[[temp_col_match]] <- rep(TRUE, nrow(matches_join_out))

  ## if resolve-type join
  if (grepl("^resolve", type)) {
    matches_join_out <- resolve_join(
      matches_join_out,
      by_ref,
      temp_col_id,
      consistent = "max"
    )
  }

  ## merge raw with final match data
  raw_join_out <- raw_join[,names_raw_prep, drop = FALSE]
  matches_out <- dplyr::left_join(raw_join_out, matches_join_out, by = temp_col_id)

  ## execute match type and remove temporary columns
  prep_output(
    x = matches_out,
    type = gsub("^resolve_", "", type),
    temp_col_id = temp_col_id,
    temp_col_match = temp_col_match,
    cols_raw_orig = names_raw_orig,
    class_raw = class_raw,
    by_raw = by_raw,
    by_ref = by_ref
  )
}


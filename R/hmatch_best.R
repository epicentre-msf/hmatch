#' Best-possible hierarchical matching
#'
#' @description
#' Match a data.frame with raw, potentially messy hierarchical data (e.g.
#' province, county, township) against a reference dataset, using a rolling
#' approach to identify the best-possible (i.e. highest-resolution) match for
#' each row.
#'
#' The rolling approach implements \code{\link{hmatch_partial}} at each
#' successive level, starting with only the first level, then the first and
#' second level, etc. The 'best-possible' match reflects the highest-level that
#' is consistent among all possible matches to the given row of raw data.
#'
#' @inheritParams hmatch_partial
#'
#' @param type type of join ("left", "inner", "anti", "inner_complete",
#'   "inner_incomplete"). Defaults to "left". See \link{join_types}.
#'
#' @return a data frame obtained by matching the hierarchical columns in `raw`
#'   and `ref`, using the join type specified by argument `type` (see
#'   \link{join_types} for more details)
#'
#' Column `match_type` indicates the type of match made:
#' - NA: no match
#' - "best": the level of the best match from `ref` matches the level of `raw`
#' - "best_low": the level of the best match from `ref` is lower than the level
#' of `raw`
#' - "best_infer": multiple rows of `ref` match `raw`; the best match
#' corresponds to the highest level common among all matches
#'
#' @examples
#' data(ne_raw)
#' data(ne_ref)
#'
#' hmatch_best(ne_raw, ne_ref, pattern = "adm", fuzzy = TRUE)
#'
#' # with dictionary-based recoding
#' ne_dict <- data.frame(value = "USA",
#'                       replacement = "United States",
#'                       variable = "adm0")
#'
#' hmatch_best(ne_raw, ne_ref, dict = ne_dict, fuzzy = TRUE)
#'
#' @importFrom dplyr left_join
#' @export hmatch_best
hmatch_best <- function(raw,
                        ref,
                        pattern = NULL,
                        pattern_ref = pattern,
                        by = NULL,
                        by_ref = by,
                        type = "left",
                        dict = NULL,
                        ref_prefix = "ref_",
                        fuzzy = FALSE,
                        max_dist = 1L,
                        std_fn = string_std,
                        ...) {

  # # for testing purposes only
  # raw <- ne_raw
  # ref <- ne_ref
  # pattern = NULL
  # pattern_ref = pattern
  # by = NULL
  # by_ref = by
  # dict = NULL
  # type = "left"
  # ref_prefix = "ref_"
  # fuzzy = TRUE
  # max_dist = 1L
  # std_fn = string_std
  # ... <- NULL

  ## match args
  if (!is.null(std_fn)) std_fn <- match.fun(std_fn)
  type <- match.arg(type, c("left", "inner", "anti", "inner_complete", "inner_incomplete"))

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
  hmatch_best_(
    raw_join = raw_join,
    ref_join = ref_join,
    by_raw = prep$by_raw,
    by_ref = prep$by_ref,
    by_raw_join = prep$by_raw_join,
    by_ref_join = prep$by_ref_join,
    type = type,
    fuzzy = fuzzy,
    max_dist = max_dist,
    class_raw = class(raw)
  )
}


#' @noRd
#' @importFrom dplyr left_join
hmatch_best_ <- function(raw_join,
                         ref_join,
                         by_raw,
                         by_ref,
                         by_raw_join,
                         by_ref_join,
                         temp_col_code = NULL,
                         type = "left",
                         fuzzy = FALSE,
                         max_dist = 1L,
                         class_raw = "data.frame") {


  ## temporary columns to aid in matching
  temp_col_id <- "TEMP_ROW_ID_SETTLE"
  raw_join[[temp_col_id]] <- seq_len(nrow(raw_join))

  if (is.null(temp_col_code)) {
    temp_col_code <- "TEMP_CODE_COL_SETTLE"
    ref_join[[temp_col_code]] <- hcodes_str(ref_join, by = by_ref)
  }

  ## re-derive initial column names
  names_raw_prep <- setdiff(names(raw_join), by_raw_join)
  names_raw_orig <- setdiff(names_raw_prep, temp_col_id)
  names_ref_prep <- setdiff(names(ref_join), by_ref_join)

  ## temp column to track max level of ref
  temp_col_max_level <- "TEMP_MAX_LEVEL_SETTLE"
  ref_join[[temp_col_max_level]] <- max_levels(ref_join, by = by_ref)

  ## initial df to store all temporary match columns
  matches_full <- raw_join[, c(by_raw, temp_col_id), drop = FALSE]

  ## find matches with hmatch_partial() at each match level...
  for (j in rev(seq_along(by_raw))) {

    # temp name for code_col at focal level
    temp_col_code_lev <- paste0(temp_col_code, j)

    # columns to exclude (> focal level)
    j_excl <- setdiff(seq_along(by_raw), seq_len(j))

    cols_excl_raw <- c(by_raw[j_excl], by_raw_join[j_excl])
    cols_excl_ref <- c(by_ref[j_excl], by_ref_join[j_excl])

    # subset to focal columns of raw and ref
    raw_foc <- raw_join[,!names(raw_join) %in% cols_excl_raw, drop = FALSE]
    ref_foc <- ref_join[,!names(raw_join) %in% cols_excl_ref, drop = FALSE]

    # filter ref to rows where max_level is <= the focal level
    ref_foc <- ref_foc[ref_foc[[temp_col_max_level]] <= j,]

    # rename code_col to focal level
    ref_foc <- rename_col(
      ref_foc,
      col_old = temp_col_code,
      col_new = temp_col_code_lev
    )

    # match raw to ref at given level
    matches_level <-  hmatch_partial_(
      raw_join = raw_foc,
      ref_join = ref_foc,
      by_raw_join = by_raw_join[1:j],
      by_ref_join = by_ref_join[1:j],
      type = "left",
      class_raw = class_raw,
      fuzzy = fuzzy,
      max_dist = max_dist
    )

    # compile matches at each level
    # TODO: test whether this fails given two adm0 with e.g. identical adm1/adm2
    #  combinations
    matches_full <- dplyr::left_join(
      matches_full,
      matches_level[,c(temp_col_id, temp_col_code_lev)],
      by = temp_col_id
    )
  }

  ## row ids from raw with >1 match in ref
  ids_dup <- matches_full[[temp_col_id]][duplicated(matches_full[[temp_col_id]])]

  ## best geocode for single-matches
  matches_single <- matches_full[!matches_full[[temp_col_id]] %in% ids_dup,]
  matches_single[[temp_col_code]] <- best_geocode(matches_single, pattern = temp_col_code)
  matches_single <- matches_single[!is.na(matches_single[[temp_col_code]]), c(temp_col_id, temp_col_code)]
  matches_single <- add_column(matches_single, "match_type", "best")

  ## best geocode for multiple-matches
  matches_multi_init <- matches_full[matches_full[[temp_col_id]] %in% ids_dup,]

  if (nrow(matches_multi_init) > 0) {

    matches_multi_split <- split(
      matches_multi_init,
      matches_multi_init[[temp_col_id]]
    )

    matches_multi_geocode <- lapply(
      matches_multi_split,
      best_geocode_helper,
      pattern = temp_col_code,
      code_col = temp_col_code,
      id_col = temp_col_id
    )

    matches_multi <- do.call(rbind.data.frame, matches_multi_geocode)
  } else {
    matches_multi <- matches_multi_init
  }

  matches_multi <- matches_multi[!is.na(matches_multi[[temp_col_code]]),]
  matches_multi <- add_column(matches_multi, "match_type", "best_infer")

  ## combine single and multiple matches
  matches_bind <- rbind_dfs(matches_single, matches_multi)

  ## join to ref
  matches_bind_ref <- dplyr::left_join(
    matches_bind,
    ref_join[,names_ref_prep, drop = FALSE],
    by = temp_col_code
  )
  matches_bind_ref <- matches_bind_ref[,c(temp_col_id, names_ref_prep, "match_type")]

  ## join to raw
  out <- dplyr::left_join(
    raw_join[,names_raw_prep, drop = FALSE],
    matches_bind_ref,
    by = temp_col_id
  )

  ## reclassify match_type (best, best_low, best_infer)
  max_adm_raw <- max_levels(out, by = by_raw)
  max_adm_ref <- max_levels(out, by = by_ref)

  best_low <- out[["match_type"]] == "best" & max_adm_ref < max_adm_raw
  best_low[is.na(best_low)] <- FALSE
  out[["match_type"]][best_low] <- "best_low"

  ## execute match type and remove temporary columns
  prep_output(
    x = out,
    type = type,
    temp_col_id = temp_col_id,
    temp_col_match = temp_col_code,
    cols_raw_orig = names_raw_orig,
    class_raw = class_raw,
    by_raw = by_raw,
    by_ref = by_ref
  )
}


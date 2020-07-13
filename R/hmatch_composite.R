#' Implement a variety of hierarchical matching strategies in sequence
#'
#' @description
#' Match a data frame with raw, potentially messy hierarchical data (e.g.
#' province, county, township) against a reference dataset, using a variety of
#' matching strategies implemented in sequence to identify the best-possible
#' match (i.e. highest-resolution) for each row.
#'
#' The sequence of matching strategies is:
#' 1. (optional) manually-specified matching with \code{\link{hmatch_manual}}
#' 2. complete matching with \code{\link{hmatch_partial}} (`allow_gaps = FALSE`)
#' 3. partial matching with \code{\link{hmatch_partial}} (`allow_gaps = TRUE`)
#' 4. fuzzy partial matching with \code{\link{hmatch_partial}} (`allow_gaps = TRUE`, `fuzzy = TRUE`)
#' 5. best-possible matching with \code{\link{hmatch_best}}
#'
#' Each approach is implement only on the rows of data for which a single match
#' has not already been identified using the previous approaches.
#'
#' @inheritParams hmatch_best
#'
#' @param man (optional) data frame of manually-specified matches, relating a
#'   given set of hierarchical values to the code within `ref` to which those
#'   values correspond
#' @param pattern regex pattern to match the hierarchical columns in `raw`
#'   (and `man` if given) (see also \link{specifying_columns})
#' @param by vector giving the names of the hierarchical columns in `raw` (and
#'   `man` if given)
#' @param code_col name of the code column containing codes for matching `ref`
#'   and `man` (only required if argument `man` is given)
#' @param type type of join ("resolve_left", "resolve_inner", or
#'   "resolve_anti"). Defaults to "left". See \link{join_types}.
#'
#' @return a data frame obtained by matching the hierarchical columns in `raw`
#'   and `ref`, using the join type specified by argument `type` (see
#'   \link{join_types} for more details)
#'
#' @examples
#' data(ne_raw)
#' data(ne_ref)
#'
#' hmatch_composite(ne_raw, ne_ref, fuzzy = TRUE)
#'
#' # with dictionary-based recoding
#' ne_dict <- data.frame(value = "USA",
#'                       replacement = "United States",
#'                       variable = "adm0")
#'
#' hmatch_composite(ne_raw, ne_ref, dict = ne_dict, fuzzy = TRUE)
#'
#' @importFrom dplyr left_join
#' @export hmatch_composite
hmatch_composite <- function(raw,
                             ref,
                             man = NULL,
                             pattern = NULL,
                             pattern_ref = pattern,
                             by = NULL,
                             by_ref = by,
                             code_col = NULL,
                             type = "resolve_left",
                             allow_gaps = TRUE,
                             fuzzy = FALSE,
                             max_dist = 1L,
                             dict = NULL,
                             ref_prefix = "ref_",
                             std_fn = string_std,
                             ...) {

  # # for testing only
  # raw <- ne_raw
  # ref <- ne_ref
  # man <- data.frame(
  #   adm0 = NA_character_,
  #   adm1 = NA_character_,
  #   adm2 = "NJ_Bergen",
  #   hcode = "211",
  #   stringsAsFactors = FALSE
  # )
  # pattern = NULL
  # pattern_ref = pattern
  # by = NULL
  # dict <- NULL
  # type <- "resolve_left"
  # code_col <- "hcode"
  # ref_prefix = "ref_"
  # fuzzy = FALSE
  # max_dist = 1L
  # std_fn = string_std
  # ... <- NULL


  ## match args
  if (!is.null(std_fn)) std_fn <- match.fun(std_fn)
  type <- match.arg(type, c("resolve_left", "resolve_inner", "resolve_anti"))

  ## save original colnames of raw
  names_raw_orig <- names(raw)

  ## temporary columns to aid in matching
  temp_col_id <- "TEMP_ROW_ID_COMPOSITE"
  temp_col_code <- "TEMP_COL_CODE_COMPOSITE"

  raw[[temp_col_id]] <- seq_len(nrow(raw))

  ## identify hierarchical columns to match, and rename ref cols if necessary
  prep <- prep_match_columns(
    raw = raw,
    ref = ref,
    pattern = pattern,
    pattern_ref = pattern_ref,
    by = by,
    by_ref = by_ref,
    ref_prefix = ref_prefix,
    code_col = temp_col_code
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

  ## initiate empty match dfs
  m_manual <- m_complete <- m_partial <- m_fuzzy <- m_settle <- NULL

  ## initial df to store remaining unmatched rows of raw
  raw_join_remaining <- raw_join

  ## manual match
  if (!is.null(man)) {

    ## join ref to man by code_col
    man_ref <- dplyr::inner_join(
      prep$ref,
      man,
      by = code_col
    )

    man_join <- add_join_columns(
      dat = man_ref,
      by = prep$by_raw,
      join_cols = prep$by_raw_join,
      std_fn = std_fn,
      ...
    )

    ## run main matching routines
    m_manual <- hmatch_manual_(
      raw_join = raw_join,
      man_join = man_join,
      by_raw = prep$by_raw,
      by_ref = prep$by_ref,
      by_join = prep$by_raw_join,
      type = "inner",
      class_raw = class(raw)
    )

    m_manual <- m_manual[,c(temp_col_id, temp_col_code)]
    m_manual$match_type <- rep("manual", nrow(m_manual))

    unmatched <- !raw_join_remaining[[temp_col_id]] %in% m_manual[[temp_col_id]]
    raw_join_remaining <- raw_join_remaining[unmatched,]
  }

  ## complete non-fuzzy match
  if (nrow(raw_join_remaining) > 0) {

    m_complete <- hmatch_partial_(
      raw_join = raw_join_remaining,
      ref_join = ref_join,
      by_raw = prep$by_raw,
      by_ref = prep$by_ref,
      by_raw_join = prep$by_raw_join,
      by_ref_join = prep$by_ref_join,
      type = "resolve_inner",
      allow_gaps = FALSE,
      fuzzy = FALSE
    )

    m_complete <- m_complete[,c(temp_col_id, temp_col_code)]
    m_complete$match_type <- rep("complete", nrow(m_complete))

    unmatched <- !raw_join_remaining[[temp_col_id]] %in% m_complete[[temp_col_id]]
    raw_join_remaining <- raw_join_remaining[unmatched,]
  }

  ## partial non-fuzzy match
  if (nrow(raw_join_remaining) > 0 & allow_gaps) {

    m_partial <- hmatch_partial_(
      raw_join = raw_join_remaining,
      ref_join = ref_join,
      by_raw = prep$by_raw,
      by_ref = prep$by_ref,
      by_raw_join = prep$by_raw_join,
      by_ref_join = prep$by_ref_join,
      type = "resolve_inner",
      allow_gaps = TRUE,
      fuzzy = FALSE
    )

    m_partial <- m_partial[,c(temp_col_id, temp_col_code)]
    m_partial$match_type <- rep("partial", nrow(m_partial))

    unmatched <- !raw_join_remaining[[temp_col_id]] %in% m_partial[[temp_col_id]]
    raw_join_remaining <- raw_join_remaining[unmatched,]
  }

  ## partial fuzzy match
  if (nrow(raw_join_remaining) > 0) {

    m_fuzzy <- hmatch_partial_(
      raw_join = raw_join_remaining,
      ref_join = ref_join,
      by_raw = prep$by_raw,
      by_ref = prep$by_ref,
      by_raw_join = prep$by_raw_join,
      by_ref_join = prep$by_ref_join,
      type = "resolve_inner",
      allow_gaps = allow_gaps,
      fuzzy = TRUE,
      max_dist = max_dist
    )

    m_fuzzy <- m_fuzzy[,c(temp_col_id, temp_col_code)]
    m_fuzzy$match_type <- rep("fuzzy", nrow(m_fuzzy))

    unmatched <- !raw_join_remaining[[temp_col_id]] %in% m_fuzzy[[temp_col_id]]
    raw_join_remaining <- raw_join_remaining[unmatched,]
  }

  ## settle join
  if (nrow(raw_join_remaining) > 0) {

    m_settle <- hmatch_best_(
      raw_join = raw_join_remaining,
      ref_join = ref_join,
      by_raw = prep$by_raw,
      by_ref = prep$by_ref,
      by_raw_join = prep$by_raw_join,
      by_ref_join = prep$by_ref_join,
      allow_gaps = allow_gaps,
      type = "resolve_inner",
      fuzzy = fuzzy,
      max_dist = max_dist
    )

    m_settle <- m_settle[,c(temp_col_id, temp_col_code)]
    m_settle$match_type <- rep("settle", nrow(m_settle))
  }

  ## combine results from all match types
  m_full <- rbind.data.frame(
    m_manual,
    m_complete,
    m_partial,
    m_fuzzy,
    m_settle
  )

  ## merge to ref
  m_bind_ref <- dplyr::left_join(m_full, prep$ref, by = temp_col_code)
  m_bind_ref <- m_bind_ref[,c(temp_col_id, names(prep$ref), "match_type")]

  ## merge to raw
  out <- dplyr::left_join(raw, m_bind_ref, by = temp_col_id)

  ## execute match type and remove temporary columns
  prep_output(
    x = out,
    type = gsub("^resolve_", "", type),
    temp_col_id = temp_col_id,
    temp_col_match = temp_col_code,
    cols_raw_orig = names_raw_orig,
    class_raw = class(raw),
    by_raw = prep$by_raw,
    by_ref = prep$by_ref
  )
}


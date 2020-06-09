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
                        concise = FALSE,
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

  if (!is.null(std_fn)) std_fn <- match.fun(std_fn)
  type <- match.arg(type, c("left", "inner", "anti", "inner_complete", "inner_incomplete"))

  # names of temporary columns
  code_col <- "TEMP_CODE_COL_ROLL"
  id_col <- "TEMP_ROW_ID_ROLL"

  ## identify hierarchical columns to match, and rename ref cols if necessary
  prep <- prep_match_columns(raw = raw,
                             ref = ref,
                             pattern = pattern,
                             pattern_ref = pattern_ref,
                             by = by,
                             by_ref = by_ref,
                             ref_prefix = ref_prefix,
                             code_col = code_col)

  ## prepare match, code, and ID columns
  by <- prep$by_raw
  by_ref <- prep$by_ref

  raw[[id_col]] <- seq_len(nrow(raw))

  raw_ <- raw[c(id_col, prep$by_raw)]
  ref_ <- prep$ref[c(prep$by_ref, code_col)]

  ref_[["MAX_LEVEL"]] <- max_levels(ref_, by = prep$by_ref)

  ## initial df to store all temporary match columns
  matches_full <- raw

  ## find matches with hmatch_partial() at each match level...
  for (j in rev(seq_along(by))) {

    # temp name for code_col at focal level
    code_col_lev <- paste0(code_col, j)

    # columns to exclude (> focal level)
    j_excl <- setdiff(seq_along(by), seq_len(j))
    cols_excl_raw <- prep$by_raw[j_excl]
    cols_excl_ref <- prep$by_ref[j_excl]

    # subset to focal columns of raw and ref
    raw_foc <- raw_[,!names(raw_) %in% cols_excl_raw, drop = FALSE]
    ref_foc <- ref_[,!names(ref_) %in% cols_excl_ref, drop = FALSE]

    # filter ref to rows where max_level is <= the focal level
    ref_foc <- ref_foc[ref_foc[["MAX_LEVEL"]] <= j,]

    # rename code_col to focal level
    ref_foc <- rename_col(ref_foc,
                          col_old = code_col,
                          col_new = code_col_lev)

    # match raw to ref at given level
    matches_level <- hmatch_partial(raw_foc,
                                    ref_foc,
                                    by = by[1:j],
                                    by_ref = by_ref[1:j],
                                    dict = dict,
                                    type = "left",
                                    fuzzy = fuzzy,
                                    max_dist = max_dist,
                                    std_fn = std_fn,
                                    ...)

    # compile matches at each level
    # TODO: test whether this fails given two adm0 with e.g. identical adm1/adm2
    #  combinations
    matches_full <- dplyr::left_join(matches_full,
                                     matches_level[,c(id_col, code_col_lev)],
                                     by = id_col)
  }

  ## row ids from raw with >1 match in ref
  ids_dup <- matches_full[[id_col]][duplicated(matches_full[[id_col]])]

  ## best geocode for single-matches
  matches_single <- matches_full[!matches_full[[id_col]] %in% ids_dup,]
  matches_single[[code_col]] <- best_geocode(matches_single, pattern = code_col)
  matches_single <- matches_single[!is.na(matches_single[[code_col]]), c(id_col, code_col)]
  matches_single <- add_column(matches_single, "match_type", "best")

  ## best geocode for multiple-matches
  matches_multi_init <- matches_full[matches_full[[id_col]] %in% ids_dup,]

  if (nrow(matches_multi_init) > 0) {

    matches_multi_split <- split(matches_multi_init,
                                 matches_multi_init[[id_col]])

    matches_multi_geocode <- lapply(matches_multi_split,
                                    best_geocode_helper,
                                    pattern = code_col,
                                    code_col = code_col,
                                    id_col = id_col)

    matches_multi <- do.call(rbind.data.frame, matches_multi_geocode)
  } else {
    matches_multi <- matches_multi_init
  }

  matches_multi <- matches_multi[!is.na(matches_multi[[code_col]]),]
  matches_multi <- add_column(matches_multi, "match_type", "best_infer")

  ## combine single and multiple matches
  matches_bind <- rbind_dfs(matches_single, matches_multi)

  ## join to ref
  matches_bind_ref <- dplyr::left_join(matches_bind, prep$ref, by = code_col)
  matches_bind_ref <- matches_bind_ref[,c(id_col, names(prep$ref), "match_type")]

  ## join to raw
  out <- dplyr::left_join(raw, matches_bind_ref, by = id_col)

  ## reclassify match_type (best, best_low, best_infer)
  max_adm_raw <- max_levels(out, by = prep$by_raw)
  max_adm_ref <- max_levels(out, by = prep$by_ref)

  best_low <- out[["match_type"]] == "best" & max_adm_ref < max_adm_raw
  best_low[is.na(best_low)] <- FALSE
  out[["match_type"]][best_low] <- "best_low"

  ## execute match type
  if (type == "inner") {
    out <- out[!is.na(out$match_type),]
  } else if (type == "anti") {
    out <- out[is.na(out$match_type), names(raw)]
  } else if (type == "inner_complete") {
    out <- out[max_adm_ref == max_adm_raw,]
  } else if (type == "inner_incomplete") {
    out <- out[max_adm_ref < max_adm_raw,]
  }

  # reclass out to match raw (tibble classes with otherwise be stripped)
  class(out) <- class(raw)

  ## remove temporary columns and return
  out <- out[,!names(out) %in% c(id_col, code_col), drop = FALSE]
  if (concise) out <- out[,c(prep$by_raw, prep$by_ref)]

  return(out)
}

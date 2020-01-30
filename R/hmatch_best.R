#' Find the highest-resolution hierarchical match using a variety of matching
#' techniques
#'
#' Match a data.frame with raw, potentially messy hierarchical data (e.g.
#' province, county, township) against a reference dataset, using a
#' variety of matching strategies to identify the best-possible match (i.e.
#' highest-resolution) for each row.
#'
#' @param raw `data.frame` containing hierarchical columns with raw, potentially
#'   messy data
#' @param ref `data.frame` containing hierarchical columns with reference data
#' @param man `data.frame` of manually-specified matches, relating a given set
#'   of hierarchical values to the code within `ref` to which those values
#'   correspond
#' @param pattern_raw regex pattern to match the hierarchical columns in `raw`
#' @param pattern_ref regex pattern to match the hierarchical columns in `ref`
#' @param pattern_man regex pattern to match the hierarchical columns in `man`
#' @param by named character vector whose elements are the names of the
#'   geo-columns in `raw` and whose names are the names of the corresponding
#'   geo-columns in `ref` (and `man`, if given)
#' @param code_col name of the code column within `ref` (and `man`, if given)
#' @param std_fn Function to standardize strings during matching. Defaults to
#'   \code{\link{string_std}}. Set to `NULL` to omit standardization. See
#'   also \link{string_standardization}.
#' @param fuzzy logical indicating whether to use fuzzy-matching (defaults to
#'   FALSE)
#' @param max_dist if `fuzzy = TRUE`, the maximum string distance to use when
#'   fuzzy-matching (defaults to `1L`)
#'
#' @return A `data.frame` obtained by matching the hierarchical columns in `raw`
#'   and `ref`, based on the matches specified in `man`. If `type == "inner"`,
#'   returns only the rows of `raw` with a single match in `ref`. If `type ==
#'   "left"`, returns all rows of `raw`. If the hierarchical columns within
#'   `ref` have identical names to `raw`, the returned reference columns will be
#'   renamed with prefix "bind_".
#'
#' @examples
#' data(drc_raw)
#' data(drc_ref)
#'
#' hmatch_best(drc_raw, drc_ref, code_col = "pcode")
#'
#' @importFrom stats setNames
#' @importFrom tidyselect all_of
#' @import rlang dplyr
#' @export hmatch_best
hmatch_best <- function(raw,
                         ref,
                         man = NULL,
                         pattern_raw = NULL,
                         pattern_ref = pattern_raw,
                         pattern_man = pattern_raw,
                         by = NULL,
                         code_col,
                         std_fn = string_std,
                         fuzzy = FALSE,
                         max_dist = 1L) {

  # raw <- drc_raw
  # ref <- drc_ref
  # man <- NULL
  # pattern_raw = NULL
  # pattern_ref = pattern_raw
  # pattern_man = pattern_raw
  # by = NULL
  # code_col <- "pcode"
  # fuzzy = FALSE
  # max_dist = 1L

  if (!is.null(std_fn)) std_fn <- match.fun(std_fn)

  raw$ROW_ID_TEMP <- 1:nrow(raw)

  ## manual match
  if (!is.null(man)) {

    m_manual <- hmatch_manual(raw,
                              ref,
                              man,
                              pattern_raw = pattern_raw,
                              pattern_ref = pattern_ref,
                              pattern_man = pattern_man,
                              by = by,
                              code_col = code_col,
                              type = "inner",
                              std_fn = std_fn)

    m_manual <- m_manual[,c("ROW_ID_TEMP", code_col)]
    m_manual$match_type <- "manual"
    raw_no_manual <- raw[!raw$ROW_ID_TEMP %in% m_manual$ROW_ID_TEMP,]

  } else {
    m_manual <- NULL
    raw_no_manual <- raw
  }

  ## prep ref
  list_prep_ref <- prep_ref(raw = raw,
                            ref = ref,
                            pattern_raw = pattern_raw,
                            pattern_ref = pattern_ref,
                            by = by)

  ref <- list_prep_ref$ref
  by_raw <- list_prep_ref$by_raw
  by_ref <- list_prep_ref$by_ref

  by <- setNames(by_ref, by_raw)
  max_level <- length(by)

  ## exact match
  m_exact <- hmatch_exact(raw_no_manual, ref, by = by, type = "inner", std_fn = std_fn)
  m_exact <- m_exact[,c("ROW_ID_TEMP", code_col)]
  m_exact$match_type <- if(nrow(m_exact) > 0) "exact" else character(0)
  raw_no_exact <- raw_no_manual[!raw_no_manual$ROW_ID_TEMP %in% m_exact$ROW_ID_TEMP,]

  ## partial join
  m_partial <- hmatch_partial(raw_no_exact, ref, by = by, type = "inner", std_fn = std_fn)
  m_partial <- m_partial[,c("ROW_ID_TEMP", code_col)]
  m_partial$match_type <- if(nrow(m_partial) > 0) "partial" else character(0)
  raw_no_partial <- raw_no_exact[!raw_no_exact$ROW_ID_TEMP %in% m_partial$ROW_ID_TEMP,]

  ## partial-fuzzy join
  m_fuzzy <- hmatch_partial(raw_no_partial, ref, by = by, type = "inner", std_fn = std_fn, fuzzy = TRUE)
  m_fuzzy <- m_fuzzy[,c("ROW_ID_TEMP", code_col)]
  m_fuzzy$match_type <- if(nrow(m_fuzzy) > 0) "fuzzy" else character(0)
  raw_no_fuzzy <- raw_no_partial[!raw_no_partial$ROW_ID_TEMP %in% m_fuzzy$ROW_ID_TEMP,]

  raw_all_codes <- raw_no_fuzzy

  for (j in max_level:1) {

    raw_excl <- by_raw[!by_raw %in% by_raw[1:j]]
    ref_excl <- by_ref[!by_ref %in% by_ref[1:j]]

    code_lev <- paste0(code_col, j)

    raw_foc <- raw_no_fuzzy[,!names(raw_no_fuzzy) %in% raw_excl]

    ref_foc <- ref[,c(by_ref[1:j], code_col)] %>%
      dplyr::filter(max_adm_level(ref[,by_ref]) <= j) %>%
      dplyr::rename(!!!setNames(code_col, code_lev))

    raw_all_codes <- hmatch_partial(raw_foc,
                                     ref_foc,
                                     by = by[1:j],
                                     type = "left",
                                     std_fn = std_fn,
                                     fuzzy = fuzzy,
                                     max_dist = max_dist) %>%
      dplyr::select("ROW_ID_TEMP", all_of(code_lev)) %>%
      dplyr::right_join(raw_all_codes, by = "ROW_ID_TEMP")
  }

  raw_best <- sort_cols(raw_all_codes, raw)
  raw_best$conflict <- test_geocode_conflict(raw_best, code_col)

  dup_ids <- raw_best$ROW_ID_TEMP[duplicated(raw_best$ROW_ID_TEMP)]

  ## get best pcode
  raw_best_single <- raw_best[!raw_best$ROW_ID_TEMP %in% dup_ids,]
  raw_best_single$pcode <- best_geocode(raw_best_single, code_col)
  raw_best_single$match_type <- if(nrow(raw_best_single) > 0) "best_single" else character(0)
  raw_best_single <- raw_best_single[!is.na(raw_best_single[[code_col]]),c("ROW_ID_TEMP", code_col, "match_type")]

  raw_best_multiple <- raw_best[raw_best$ROW_ID_TEMP %in% dup_ids,]

  if (nrow(raw_best_multiple) > 0) {
    raw_best_multiple <- raw_best_multiple %>%
      group_by(!!sym("ROW_ID_TEMP")) %>%
      do(best_geocode_helper(.data, pattern = code_col)) %>%
      ungroup()
  }

  raw_best_multiple$match_type <- if(nrow(raw_best_multiple) > 0) "best_multi" else character(0)
  raw_best_multiple <- raw_best_multiple[!is.na(raw_best_multiple[[code_col]]),]

  ref_bind <- dplyr::bind_rows(m_manual,
                               m_exact,
                               m_partial,
                               m_fuzzy,
                               raw_best_single,
                               raw_best_multiple)

  ref_bind <- ref_bind[order(ref_bind$ROW_ID_TEMP),] %>%
    dplyr::left_join(ref, by = code_col) %>%
    dplyr::select("ROW_ID_TEMP", all_of(by_ref), "pcode", "match_type")

  out <- dplyr::left_join(raw, ref_bind, by = "ROW_ID_TEMP")

  return(out[,!names(out) %in% "ROW_ID_TEMP", drop = FALSE])
}


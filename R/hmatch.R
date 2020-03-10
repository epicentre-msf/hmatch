#' Implement a variety of hierarchical matching strategies in sequence
#'
#' @description
#' Match a data.frame with raw, potentially messy hierarchical data (e.g.
#' province, county, township) against a reference dataset, using a variety of
#' matching strategies implemented in sequence to identify the best-possible
#' match (i.e. highest-resolution) for each row.
#'
#' The sequence of matching stragies is:
#' 1. (optional) manually-specified matching with \code{\link{hmatch_manual}}
#' 2. exact matching with \code{\link{hmatch_exact}}
#' 3. partial matching with \code{\link{hmatch_partial}}
#' 4. fuzzy partial matching with \code{\link{hmatch_partial}}
#' 5. best-possible matching with \code{\link{hmatch_best}}
#'
#' Each approach is implement only on the rows of data for which a single match
#' has not already been identified using the previous approaches.
#'
#' @param raw `data.frame` containing hierarchical columns with raw, potentially
#'   messy data
#' @param ref `data.frame` containing hierarchical columns with reference data
#' @param man (optional) `data.frame` of manually-specified matches, relating a
#'   given set of hierarchical values to the code within `ref` to which those
#'   values correspond
#' @param pattern_raw regex pattern to match the hierarchical columns in `raw`
#'   (and `man` if given) (see also \link{specifying_columns})
#' @param pattern_ref regex pattern to match the hierarchical columns in `ref`
#'   (see also \link{specifying_columns})
#' @param by named character vector whose elements are the names of the
#'   hierarchical columns in `ref`, and whose names are the names of the
#'   corresponding columns in `raw` (and `man`, if given) (see also
#'   \link{specifying_columns})
#' @param code_col name of the code column containing codes for matching `ref`
#'   and `man` (only required if argument `man` is given)
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
#' hmatch(drc_raw, drc_ref)
#'
#' @importFrom stats setNames
#' @importFrom tidyselect all_of
#' @import rlang dplyr
#' @export hmatch
hmatch <- function(raw,
                   ref,
                   man = NULL,
                   pattern_raw = NULL,
                   pattern_ref = pattern_raw,
                   by = NULL,
                   code_col = NULL,
                   std_fn = string_std,
                   fuzzy = FALSE,
                   max_dist = 1L) {

  if (!is.null(std_fn)) std_fn <- match.fun(std_fn)

  raw$TEMP_ROW_ID_BEST <- seq_len(nrow(raw))

  list_prep_ref <- prep_ref(raw = raw,
                            ref = ref,
                            pattern_raw = pattern_raw,
                            pattern_ref = pattern_ref,
                            by = by)


  m_manual <- m_exact <- m_partial <- m_fuzzy <- m_roll <- NULL
  raw_remaining <- raw

  ## manual match
  if (!is.null(man)) {

    m_manual <- hmatch_manual(raw,
                              ref,
                              man,
                              pattern_raw = pattern_raw,
                              pattern_ref = pattern_ref,
                              by = by,
                              code_col = code_col,
                              type = "inner",
                              std_fn = std_fn)

    m_manual$TEMP_CODE_COL_BEST <- hcodes_str(m_manual, by = list_prep_ref$by_ref)

    m_manual <- m_manual[,c("TEMP_ROW_ID_BEST", "TEMP_CODE_COL_BEST")]
    m_manual$match_type <- if (nrow(m_manual) > 0) "manual" else character(0)
    raw_remaining <- raw_remaining[!raw_remaining$TEMP_ROW_ID_BEST %in% m_manual$TEMP_ROW_ID_BEST,]
  }

  ref <- list_prep_ref$ref
  by_raw <- list_prep_ref$by_raw
  by_ref <- list_prep_ref$by_ref

  by <- setNames(by_ref, by_raw)
  max_level <- length(by_raw)

  code_col <- "TEMP_CODE_COL_BEST"

  ref$TEMP_CODE_COL_BEST <- hcodes_str(ref, by = by)

  ## exact match
  if (nrow(raw_remaining) > 0) {
    m_exact <- hmatch_exact(raw_remaining, ref, by = by, type = "inner", std_fn = std_fn)
    m_exact <- m_exact[,c("TEMP_ROW_ID_BEST", code_col)]
    m_exact$match_type <- if (nrow(m_exact) > 0) "exact" else character(0)
    raw_remaining <- raw_remaining[!raw_remaining$TEMP_ROW_ID_BEST %in% m_exact$TEMP_ROW_ID_BEST,]
  }

  ## partial join
  if (nrow(raw_remaining) > 0) {
    m_partial <- hmatch_partial(raw_remaining, ref, by = by, type = "inner", std_fn = std_fn)
    m_partial <- m_partial[,c("TEMP_ROW_ID_BEST", code_col)]
    m_partial$match_type <- if (nrow(m_partial) > 0) "partial" else character(0)
    raw_remaining <- raw_remaining[!raw_remaining$TEMP_ROW_ID_BEST %in% m_partial$TEMP_ROW_ID_BEST,]
  }

  ## partial-fuzzy join
  if (nrow(raw_remaining) > 0) {
    m_fuzzy <- hmatch_partial(raw_remaining, ref, by = by, type = "inner", std_fn = std_fn, fuzzy = TRUE)
    m_fuzzy <- m_fuzzy[,c("TEMP_ROW_ID_BEST", code_col)]
    m_fuzzy$match_type <- if (nrow(m_fuzzy) > 0) "fuzzy" else character(0)
    raw_remaining <- raw_remaining[!raw_remaining$TEMP_ROW_ID_BEST %in% m_fuzzy$TEMP_ROW_ID_BEST,]
  }

  ## rolling join
  if (nrow(raw_remaining) > 0) {
    m_roll <- hmatch_best(raw_remaining, ref, by = by, type = "inner",
                          std_fn = std_fn, fuzzy = fuzzy, max_dist = max_dist)
    m_roll <- m_roll[,c("TEMP_ROW_ID_BEST", code_col, "match_type")]
  }

  ## combine results
  ref_bind <- dplyr::bind_rows(m_manual,
                               m_exact,
                               m_partial,
                               m_fuzzy,
                               m_roll)

  ref_bind <- ref_bind[order(ref_bind$TEMP_ROW_ID_BEST),] %>%
    dplyr::left_join(ref, by = code_col) %>%
    dplyr::select("TEMP_ROW_ID_BEST", all_of(names(ref)), "match_type")

  out <- dplyr::left_join(raw, ref_bind, by = "TEMP_ROW_ID_BEST")

  return(out[,!names(out) %in% c("TEMP_ROW_ID_BEST", "TEMP_CODE_COL_BEST"), drop = FALSE])
}


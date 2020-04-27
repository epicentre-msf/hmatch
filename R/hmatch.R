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
#' 2. complete matching with \code{\link{hmatch_complete}}
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
#' @param ref_prefix Prefix to add to hierarchical column names in `ref` if they
#'   are otherwise identical to names in `raw`  (defaults to `ref_`)
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
#'   renamed with prefix "ref_".
#'
#' @examples
#' data(ne_raw)
#' data(ne_ref)
#'
#' hmatch(ne_raw, ne_ref, fuzzy = FALSE)
#'
#' @importFrom stats setNames
#' @export hmatch
hmatch <- function(raw,
                   ref,
                   man = NULL,
                   pattern_raw = NULL,
                   pattern_ref = pattern_raw,
                   by = NULL,
                   code_col = NULL,
                   ref_prefix = "ref_",
                   std_fn = string_std,
                   fuzzy = FALSE,
                   max_dist = 1L) {

  # # for testing
  # raw <- ne_raw
  # ref <- ne_ref
  # man <- data.frame(adm0 = NA_character_,
  #                   adm1 = NA_character_,
  #                   adm2 = "NJ_Bergen",
  #                   hcode = "211",
  #                   stringsAsFactors = FALSE)
  # pattern_raw = NULL
  # pattern_ref = pattern_raw
  # by = NULL
  # code_col <- "hcode"
  # ref_prefix = "ref_"
  # std_fn = string_std
  # fuzzy = FALSE
  # max_dist = 1L

  if (!is.null(std_fn)) std_fn <- match.fun(std_fn)

  ## names of temporary columns
  temp_id_col <- "TEMP_ROW_ID_BEST"
  temp_code_col <- "TEMP_CODE_COL_BEST"

  ## add temporary row index to raw
  raw[[temp_id_col]] <- seq_len(nrow(raw))

  ## identify hierarchical columns to match, and rename ref cols if necessary
  prep <- prep_match_columns(raw = raw,
                             ref = ref,
                             pattern_raw = pattern_raw,
                             pattern_ref = pattern_ref,
                             by = by,
                             code_col = temp_code_col)

  ## initiate empty match dfs
  m_manual <- m_complete <- m_partial <- m_fuzzy <- m_roll <- NULL

  ## initial df to store remaining unmatched rows of raw
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

    m_manual[[temp_code_col]] <- hcodes_str(m_manual, by = prep$by_ref)

    m_manual <- m_manual[,c(temp_id_col, temp_code_col)]
    m_manual <- add_column(m_manual, "match_type", "manual")
    raw_remaining <- raw_remaining[!raw_remaining[[temp_id_col]] %in% m_manual[[temp_id_col]],]
  }

  ## prepare match, code, and ID columns
  ref_ <- prep$ref[,c(prep$by_ref, temp_code_col)]
  by <- setNames(prep$by_ref, prep$by_raw)

  ## complete match
  if (nrow(raw_remaining) > 0) {
    m_complete <- hmatch_complete(raw_remaining,
                                  ref_,
                                  by = by,
                                  type = "inner",
                                  std_fn = std_fn)

    m_complete <- m_complete[,c(temp_id_col, temp_code_col)]
    m_complete <- add_column(m_complete, "match_type", "complete")
    raw_remaining <- raw_remaining[!raw_remaining[[temp_id_col]] %in% m_complete[[temp_id_col]],]
  }

  ## partial join
  if (nrow(raw_remaining) > 0) {
    m_partial <- hmatch_partial(raw_remaining,
                                ref_,
                                by = by,
                                type = "inner",
                                std_fn = std_fn)

    m_partial <- m_partial[,c(temp_id_col, temp_code_col)]
    m_partial <- add_column(m_partial, "match_type", "partial")
    raw_remaining <- raw_remaining[!raw_remaining[[temp_id_col]] %in% m_partial[[temp_id_col]],]
  }

  ## partial-fuzzy join
  if (nrow(raw_remaining) > 0) {
    m_fuzzy <- hmatch_partial(raw_remaining,
                              ref_,
                              by = by,
                              type = "inner",
                              std_fn = std_fn,
                              fuzzy = TRUE)

    m_fuzzy <- m_fuzzy[,c(temp_id_col, temp_code_col)]
    m_fuzzy <- add_column(m_fuzzy, "match_type", "fuzzy")
    raw_remaining <- raw_remaining[!raw_remaining[[temp_id_col]] %in% m_fuzzy[[temp_id_col]],]
  }

  ## best-possible join
  if (nrow(raw_remaining) > 0) {
    m_roll <- hmatch_best(raw_remaining,
                          ref_,
                          by = by,
                          type = "inner",
                          std_fn = std_fn,
                          fuzzy = fuzzy,
                          max_dist = max_dist)

    m_roll <- m_roll[,c(temp_id_col, temp_code_col, "match_type")]
  }

  ## combine results from all match types
  m_full <- rbind.data.frame(m_manual,
                             m_complete,
                             m_partial,
                             m_fuzzy,
                             m_roll)

  ## merge to ref
  m_bind_ref <- merge(m_full, prep$ref, by = temp_code_col)
  m_bind_ref <- m_bind_ref[,c(temp_id_col, names(prep$ref), "match_type")]

  ## merge to raw
  out <- merge(raw, m_bind_ref, by = temp_id_col, all.x = TRUE)

  ## remove temporary columns and return
  return(out[,!names(out) %in% c(temp_id_col, temp_code_col), drop = FALSE])
}


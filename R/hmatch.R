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
#' @inheritParams hmatch_best
#'
#' @param man (optional) `data.frame` of manually-specified matches, relating a
#'   given set of hierarchical values to the code within `ref` to which those
#'   values correspond
#' @param pattern regex pattern to match the hierarchical columns in `raw`
#'   (and `man` if given) (see also \link{specifying_columns})
#' @param by vector giving the names of the hierarchical columns in `raw` (and
#'   `man` if given)
#' @param code_col name of the code column containing codes for matching `ref`
#'   and `man` (only required if argument `man` is given)
#'
#' @return a data frame obtained by matching the hierarchical columns in `raw`
#'   and `ref`, using the join type specified by argument `type` (see
#'   \link{join_types} for more details)
#'
#' @examples
#' data(ne_raw)
#' data(ne_ref)
#'
#' hmatch(ne_raw, ne_ref, fuzzy = TRUE)
#'
#' # with dictionary-based recoding
#' ne_dict <- data.frame(value = "USA",
#'                       replacement = "United States",
#'                       variable = "adm0")
#'
#' hmatch(ne_raw, ne_ref, dict = ne_dict, fuzzy = TRUE)
#'
#' @importFrom dplyr left_join
#' @export hmatch
hmatch <- function(raw,
                   ref,
                   man = NULL,
                   pattern = NULL,
                   pattern_ref = pattern,
                   by = NULL,
                   by_ref = by,
                   type = "left",
                   dict = NULL,
                   code_col = NULL,
                   ref_prefix = "ref_",
                   fuzzy = FALSE,
                   max_dist = 1L,
                   concise = FALSE,
                   std_fn = string_std,
                   ...
                   ) {

  # raw <- readRDS("~/desktop/raw.rds")
  # ref <- readRDS("~/desktop/ref.rds")
  # man <- readRDS("~/desktop/man.rds")
  # pattern <- "adm"

  # # for testing
  # raw <- ne_raw
  # ref <- ne_ref
  # man <- data.frame(adm0 = NA_character_,
  #                   adm1 = NA_character_,
  #                   adm2 = "NJ_Bergen",
  #                   hcode = "211",
  #                   stringsAsFactors = FALSE)
  # pattern = NULL
  # pattern_ref = pattern
  # by = NULL
  # dict <- NULL
  # type <- "left"
  # code_col <- "hcode"
  # ref_prefix = "ref_"
  # fuzzy = FALSE
  # max_dist = 1L
  # std_fn = string_std
  # ... <- NULL

  if (!is.null(std_fn)) std_fn <- match.fun(std_fn)
  type <- match.arg(type, c("left", "inner", "anti", "inner_complete", "inner_incomplete"))

  ## names of temporary columns
  temp_id_col <- "TEMP_ROW_ID_BEST"
  temp_code_col <- "TEMP_CODE_COL_BEST"

  ## add temporary row index to raw
  raw[[temp_id_col]] <- seq_len(nrow(raw))

  ## identify hierarchical columns to match, and rename ref cols if necessary
  prep <- prep_match_columns(raw = raw,
                             ref = ref,
                             pattern = pattern,
                             pattern_ref = pattern_ref,
                             by = by,
                             by_ref = by_ref,
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
                              pattern = pattern,
                              pattern_ref = pattern_ref,
                              by = by,
                              by_ref = by_ref,
                              code_col = code_col,
                              type = "inner",
                              std_fn = std_fn,
                              ...)

    m_manual[[temp_code_col]] <- hcodes_str(m_manual, by = prep$by_ref)

    m_manual <- m_manual[,c(temp_id_col, temp_code_col)]
    m_manual <- add_column(m_manual, "match_type", "manual")
    raw_remaining <- raw_remaining[!raw_remaining[[temp_id_col]] %in% m_manual[[temp_id_col]],]
  }

  ## prepare match, code, and ID columns
  ref_ <- prep$ref[,c(prep$by_ref, temp_code_col)]
  by <- prep$by_raw
  by_ref <- prep$by_ref

  ## complete match
  if (nrow(raw_remaining) > 0) {
    m_complete <- hmatch_complete(raw_remaining,
                                  ref_,
                                  by = by,
                                  by_ref = by_ref,
                                  dict = dict,
                                  type = "inner_unique",
                                  std_fn = std_fn,
                                  ...)

    m_complete <- m_complete[,c(temp_id_col, temp_code_col)]
    m_complete <- add_column(m_complete, "match_type", "complete")
    raw_remaining <- raw_remaining[!raw_remaining[[temp_id_col]] %in% m_complete[[temp_id_col]],]
  }

  ## partial join
  if (nrow(raw_remaining) > 0) {
    m_partial <- hmatch_partial(raw_remaining,
                                ref_,
                                by = by,
                                by_ref = by_ref,
                                dict = dict,
                                type = "inner_unique",
                                std_fn = std_fn,
                                ...)

    m_partial <- m_partial[,c(temp_id_col, temp_code_col)]
    m_partial <- add_column(m_partial, "match_type", "partial")
    raw_remaining <- raw_remaining[!raw_remaining[[temp_id_col]] %in% m_partial[[temp_id_col]],]
  }

  ## partial-fuzzy join
  if (nrow(raw_remaining) > 0) {
    m_fuzzy <- hmatch_partial(raw_remaining,
                              ref_,
                              by = by,
                              by_ref = by_ref,
                              dict = dict,
                              type = "inner_unique",
                              fuzzy = TRUE,
                              std_fn = std_fn,
                              ...)

    m_fuzzy <- m_fuzzy[,c(temp_id_col, temp_code_col)]
    m_fuzzy <- add_column(m_fuzzy, "match_type", "partial_fuzzy")
    raw_remaining <- raw_remaining[!raw_remaining[[temp_id_col]] %in% m_fuzzy[[temp_id_col]],]
  }

  ## best-possible join
  if (nrow(raw_remaining) > 0) {
    m_roll <- hmatch_best(raw_remaining,
                          ref_,
                          by = by,
                          by_ref = by_ref,
                          dict = dict,
                          type = "inner",
                          fuzzy = fuzzy,
                          max_dist = max_dist,
                          std_fn = std_fn,
                          ...)

    m_roll <- m_roll[,c(temp_id_col, temp_code_col, "match_type")]
  }

  ## combine results from all match types
  m_full <- rbind.data.frame(m_manual,
                             m_complete,
                             m_partial,
                             m_fuzzy,
                             m_roll)


  ## merge to ref
  m_bind_ref <- dplyr::left_join(m_full, prep$ref, by = temp_code_col)
  m_bind_ref <- m_bind_ref[,c(temp_id_col, names(prep$ref), "match_type")]

  ## merge to raw
  out <- dplyr::left_join(raw, m_bind_ref, by = temp_id_col)

  ## reclassify match_type (best, best_low, best_infer)
  max_adm_raw <- max_levels(out, by = prep$by_raw)
  max_adm_ref <- max_levels(out, by = prep$by_ref)

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

  ## reclass out to match raw (tibble classes with otherwise be stripped)
  row.names(out) <- NULL
  class(out) <- class(raw)

  ## remove temporary columns and return
  out <- out[,!names(out) %in% c(temp_id_col, temp_code_col), drop = FALSE]
  if (concise) out <- out[,c(prep$by_raw, prep$by_ref)]

  return(out)
}


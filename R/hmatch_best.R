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
#' @param raw `data.frame` containing hierarchical columns with raw, potentially
#'   messy data
#' @param ref `data.frame` containing hierarchical columns with reference data
#' @param pattern_raw regex pattern to match the hierarchical columns in `raw`
#'   (see also \link{specifying_columns})
#' @param pattern_ref regex pattern to match the hierarchical columns in `ref`
#'   (see also \link{specifying_columns})
#' @param by named character vector whose elements are the names of the
#'   hierarchical columns in `ref`, and whose names are the names of the
#'   corresponding columns in `raw` (see also \link{specifying_columns})
#' @param type type of join ("inner" or "left") (defaults to "left")
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
#' data(ne_raw)
#' data(ne_ref)
#'
#' hmatch_best(ne_raw, ne_ref, fuzzy = TRUE)
#'
#' @importFrom stats setNames
#' @importFrom tidyselect all_of
#' @import rlang dplyr
#' @export hmatch_best
hmatch_best <- function(raw,
                        ref,
                        pattern_raw = NULL,
                        pattern_ref = pattern_raw,
                        by = NULL,
                        type = "left",
                        std_fn = string_std,
                        fuzzy = FALSE,
                        max_dist = 1L) {

  if (!is.null(std_fn)) std_fn <- match.fun(std_fn)

  raw$TEMP_ROW_ID_ROLL <- seq_len(nrow(raw))

  list_prep_ref <- prep_ref(raw = raw,
                            ref = ref,
                            pattern_raw = pattern_raw,
                            pattern_ref = pattern_ref,
                            by = by)

  ref <- list_prep_ref$ref
  by_raw <- list_prep_ref$by_raw
  by_ref <- list_prep_ref$by_ref

  by <- setNames(by_ref, by_raw)
  max_level <- length(by_raw)

  code_col <- "TEMP_CODE_COL_ROLL"

  ref$TEMP_CODE_COL_ROLL <- hcodes_str(ref, by = by)

  raw_full <- raw

  for (j in max_level:1) {

    raw_excl <- by_raw[!by_raw %in% by_raw[1:j]]
    ref_excl <- by_ref[!by_ref %in% by_ref[1:j]]

    code_lev <- paste0(code_col, j)

    raw_foc <- raw[,!names(raw) %in% raw_excl]

    ref_foc <- ref[,c(by_ref[1:j], code_col)] %>%
      dplyr::filter(max_adm_level(ref[,by_ref]) <= j) %>%
      dplyr::rename(!!!setNames(code_col, code_lev))

    raw_full <- hmatch_partial(raw_foc,
                               ref_foc,
                               by = by[1:j],
                               type = "left",
                               std_fn = std_fn,
                               fuzzy = fuzzy,
                               max_dist = max_dist) %>%
      dplyr::select("TEMP_ROW_ID_ROLL", all_of(code_lev)) %>%
      dplyr::right_join(raw_full, by = "TEMP_ROW_ID_ROLL")
  }

  raw_best <- sort_cols(raw_full, raw)
  # raw_best$conflict <- test_geocode_conflict(raw_best, code_col)

  dup_ids <- raw_best$TEMP_ROW_ID_ROLL[duplicated(raw_best$TEMP_ROW_ID_ROLL)]

  ## get best pcode
  raw_best_single <- raw_best[!raw_best$TEMP_ROW_ID_ROLL %in% dup_ids,]
  raw_best_single[[code_col]] <- best_geocode(raw_best_single, code_col)
  raw_best_single$match_type <- if (nrow(raw_best_single) > 0) "best_single" else character(0)
  raw_best_single <- raw_best_single[!is.na(raw_best_single[[code_col]]),c("TEMP_ROW_ID_ROLL", code_col, "match_type")]

  raw_best_multiple <- raw_best[raw_best$TEMP_ROW_ID_ROLL %in% dup_ids,]

  if (nrow(raw_best_multiple) > 0) {
    raw_best_multiple <- raw_best_multiple %>%
      group_by(!!sym("TEMP_ROW_ID_ROLL")) %>%
      do(best_geocode_helper(.data, pattern = code_col, code_col = code_col)) %>%
      ungroup()
  }

  raw_best_multiple$match_type <- if (nrow(raw_best_multiple) > 0) "best_multi" else character(0)
  raw_best_multiple <- raw_best_multiple[!is.na(raw_best_multiple[[code_col]]),]

  ref_bind <- dplyr::bind_rows(raw_best_single, raw_best_multiple)

  ref_bind <- ref_bind[order(ref_bind$TEMP_ROW_ID_ROLL),] %>%
    dplyr::left_join(ref, by = code_col) %>%
    dplyr::select("TEMP_ROW_ID_ROLL", all_of(names(ref)), "match_type")

  out <- dplyr::left_join(raw, ref_bind, by = "TEMP_ROW_ID_ROLL")

  if (type == "inner") {
    out <- out[!is.na(out$match_type),]
  }

  return(out[,!names(out) %in% c("TEMP_ROW_ID_ROLL", "TEMP_CODE_COL_ROLL"), drop = FALSE])
}


#' Partial hierarchical matching
#'
#' Match a data.frame with raw, potentially messy hierarchical data (e.g.
#' province, county, township) against a reference dataset, using partial
#' matching. "Partial" here means that one or more hierarchical levels within
#' the raw data may be missing (i.e. NA). More specifically, for a given row of
#' raw data, matches can potentially be made to a high-resolution level (e.g.
#' township) even if one or more lower-resolution levels (e.g. province) is
#' missing.
#'
#' @param raw `data.frame` containing hierarchical columns with raw, potentially
#'   messy data
#' @param ref `data.frame` containing hierarchical columns with reference data
#' @param pattern_raw regex pattern to match the hierarchical columns in `raw`
#'   (see also \link{specifying_columns})
#' @param pattern_ref regex pattern to match the hierarchical columns in `ref`
#'   (see also \link{specifying_columns})
#' @param by named character vector whose elements are the names of the
#'   hierarchical columns in `ref` and whose names are the names of the
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
#'   and `ref`. If `type == "inner"`, returns only the rows of `raw` with a
#'   single match in `ref`. If `type == "left"`, returns all rows of `raw`. If
#'   the hierarchical columns within `ref` have identical names to `raw`, the
#'   returned reference columns will be renamed with prefix "ref_".
#'
#' @examples
#' data(ne_raw)
#' data(ne_ref)
#'
#' hmatch_partial(ne_raw, ne_ref, pattern_raw = "adm", type = "inner")
#'
#' @importFrom stats setNames
#' @import rlang dplyr
#' @export hmatch_partial
hmatch_partial <- function(raw,
                           ref,
                           pattern_raw = NULL,
                           pattern_ref = pattern_raw,
                           by = NULL,
                           type = "left",
                           std_fn = string_std,
                           fuzzy = FALSE,
                           max_dist = 1L) {

  # pattern_raw = NULL
  # pattern_ref = pattern_raw
  # by = NULL
  # type = "left"
  # std_fn = string_std
  # fuzzy = FALSE
  # max_dist = 1L

  if (!is.null(std_fn)) std_fn <- match.fun(std_fn)

  list_prep_ref <- prep_ref(raw = raw,
                            ref = ref,
                            pattern_raw = pattern_raw,
                            pattern_ref = pattern_ref,
                            by = by)

  ref <- list_prep_ref$ref
  by_raw <- list_prep_ref$by_raw
  by_ref <- list_prep_ref$by_ref
  by_join <- list_prep_ref$by_join

  by_raw_join <- paste0(by_raw, "__JOIN")
  by_ref_join <- paste0(by_ref, "__JOIN")

  max_level <- length(by_raw)

  raw_cols_orig <- names(raw)
  raw$TEMP_ROW_ID_PART <- seq_len(nrow(raw))

  raw_join <- add_join_columns(raw, by_raw, join_cols = by_raw_join, std_fn = std_fn)
  ref_join <- add_join_columns(ref, by_ref, join_cols = by_ref_join, std_fn = std_fn)

  raw_ <- raw_join[,by_raw_join, drop = FALSE]
  ref_ <- ref_join[,by_ref_join, drop = FALSE]

  col_max_raw <- by_raw_join[max_level]
  col_max_ref <- by_ref_join[max_level]

  col_max_sym_raw <- rlang::sym(col_max_raw)
  col_max_sym_ref <- rlang::sym(col_max_ref)

  initial_matches <- expand.grid(x = unique(raw_[[col_max_raw]]),
                                 y = unique(ref_[[col_max_ref]]),
                                 stringsAsFactors = FALSE) %>%
    setNames(c(col_max_raw, col_max_ref)) %>%
    dplyr::as_tibble()

  if (!fuzzy) {
    initial_matches_filter <- initial_matches %>%
      dplyr::filter((is.na(!!col_max_sym_raw) & is.na(!!col_max_sym_ref)) | !!col_max_sym_raw == !!col_max_sym_ref)
  } else {
    initial_matches_filter <- initial_matches %>%
      dplyr::filter((is.na(!!col_max_sym_raw) & is.na(!!col_max_sym_ref)) |
                      stringdist::stringdist(!!col_max_sym_raw, !!col_max_sym_ref) <= max_dist)
  }

  initial_matches_join <- initial_matches_filter %>%
    dplyr::left_join(raw_join, by = col_max_raw) %>%
    dplyr::left_join(ref_join, by = col_max_ref)

  if (max_level > 1) {
    for (j in (max_level - 1):1) {
      cols_focal_raw <- names(raw_)[j]
      cols_focal_ref <- names(ref_)[j]

      cols_focal_sym_raw <- rlang::sym(cols_focal_raw)
      cols_focal_sym_ref <- rlang::sym(cols_focal_ref)

      if (!fuzzy) {
        initial_matches_join <- initial_matches_join %>%
          dplyr::filter(is.na(!!cols_focal_sym_raw) |
                          !!cols_focal_sym_raw == !!cols_focal_sym_ref)
      } else {
        initial_matches_join <- initial_matches_join %>%
          dplyr::filter(is.na(!!cols_focal_sym_raw) |
                          stringdist::stringdist(!!cols_focal_sym_raw, !!cols_focal_sym_ref) <= max_dist)
      }
    }
  }

  matches_out <- initial_matches_join[,c(raw_cols_orig, names(ref))]
  matches_out <- corresponding_levels(matches_out, by_raw, by_ref)
  matches_out$TEMP_IS_MATCH <- if (nrow(matches_out) > 0) "MATCH" else character(0)

  out <- dplyr::left_join(raw, matches_out, by = raw_cols_orig)

  if (type == "inner") {
    out <- out[!is.na(out$TEMP_IS_MATCH),]
    dup_ids <- out$TEMP_ROW_ID_PART[duplicated(out$TEMP_ROW_ID_PART)]
    out <- out[!out$TEMP_ROW_ID_PART %in% dup_ids,]
  }

  out[,!names(out) %in% c("TEMP_ROW_ID_PART", "TEMP_IS_MATCH"), drop = FALSE]
}



#' @noRd
prep_match_columns <- function(raw,
                               ref,
                               pattern,
                               pattern_ref,
                               by,
                               by_ref,
                               ref_prefix = "ref_",
                               join_suffix = "___JOIN_",
                               code_col = NULL) {


  if (missing(pattern)) pattern <- NULL
  if (missing(pattern_ref)) pattern_ref <- NULL
  if (missing(by)) by <- NULL
  if (missing(by_ref)) by_ref <- NULL

  validate_match_args(pattern, by)

  if (!is.null(by)) {
    by_raw <- select_by(raw, by)
    by_ref <- select_by(ref, by_ref)
  } else if (!is.null(pattern)) {
    by_raw <- select_pattern(raw, pattern)
    by_ref <- select_pattern(ref, pattern_ref)
  } else {
    by_raw <- intersect(names(ref), names(raw))
    by_ref <- intersect(names(ref), names(raw))

    if (length(by_raw) == 0L) {
      stop("Arguments `by` and `pattern` both missing, and no common column ",
           "names between `raw` and `ref`", call. = FALSE)
    }
  }

  by_ref_orig <- by_ref

  # rename hierarchical cols of ref if necessary
  if (all(by_raw == by_ref)) {
    by_ref <- paste0(ref_prefix, by_ref)
    names(ref)[match(by_raw, names(ref))] <- by_ref
  }

  # rename columns in ref that match raw (including hierarchical columns)
  names_intersect <- intersect(names(ref), names(raw))

  if (length(names_intersect) > 0) {
    names_intersect_prefix <- paste0(ref_prefix, names_intersect)
    names(ref)[match(names_intersect, names(ref))] <- names_intersect_prefix
  }

  by_raw_join <- paste0(by_raw, join_suffix)
  by_ref_join <- paste0(by_ref, join_suffix)

  if (!is.null(code_col)) {
    ref[[code_col]] <- hcodes_str(ref, by = by_ref)
  }

  return(list(ref = ref,
              by_raw = by_raw,
              by_ref = by_ref,
              by_ref_orig = by_ref_orig,
              by_raw_join = by_raw_join,
              by_ref_join = by_ref_join))
}


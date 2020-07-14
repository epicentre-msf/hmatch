

#' @noRd
prep_output <- function(x,
                        type,
                        temp_col_id,
                        temp_col_match,
                        cols_raw_orig,
                        class_raw,
                        by_raw, # only used in hmatch_settle
                        by_ref, # only used in hmatch_settle
                        exclude_cols_temp = TRUE) {

  x_id <- x[[temp_col_id]]
  x_match <- x[[temp_col_match]]

  ## arrange rows
  # x <- x[order(x_id), , drop = FALSE]

  ## execute merge type
  if (type == "left") {
    out <- x
  } else if (type == "inner") {
    keep <- !is.na(x_match)
    out <- x[keep, , drop = FALSE]
  } else if (type == "inner_unique") {
    ids_duplicated <- x_id[duplicated(x_id)]
    keep <- !is.na(x_match) & !x_id %in% ids_duplicated
    out <- x[keep, , drop = FALSE]
  } else if (type == "anti") {
    keep <- is.na(x_match)
    out <- x[keep, cols_raw_orig, drop = FALSE]
  } else if (type == "anti_unique") {
    ids_duplicated <- x_id[duplicated(x_id)]
    keep <- is.na(x_match) | x_id %in% ids_duplicated
    out <- unique(x[keep, cols_raw_orig, drop = FALSE])
  } else if (type == "inner_complete") {
    max_adm_raw <- max_levels(x, by = by_raw)
    max_adm_ref <- max_levels(x, by = by_ref)
    out <- x[max_adm_ref == max_adm_raw,]
  } else if (type == "inner_incomplete") {
    max_adm_raw <- max_levels(x, by = by_raw)
    max_adm_ref <- max_levels(x, by = by_ref)
    out <- x[max_adm_ref < max_adm_raw,]
  }

  ## remove temporary and excluded names
  if (exclude_cols_temp) {
    out <- out[,!names(out) %in% c(temp_col_id, temp_col_match), drop = FALSE]
  }

  ## reclass
  class(out) <- class_raw

  return(out)
}



#' @noRd
#' @importFrom dplyr bind_rows
resolve_join <- function(x, by_ref, temp_col_id, consistent = c("min", "max", "all")) {
  if (nrow(x) == 0L) {
    out <- x
  } else {
    consistent <- match.arg(consistent)
    x_split <- split(x, x[[temp_col_id]])
    l_resolve <- lapply(x_split, resolve_join_, by_ref = by_ref, consistent = consistent)
    out <- dplyr::bind_rows(l_resolve)
  }
  out
}



#' @noRd
resolve_join_ <- function(x, by_ref, consistent) {

  if (nrow(x) < 2L) {
    out <- x
  } else {

    ## 2 or more matches...
    ref_sub_ <- x[,by_ref, drop = FALSE]
    matches_consistent <- vapply(ref_sub_, unique_excl_na, FALSE)
    max_matches_consistent <- max_before_false(matches_consistent)

    if (!matches_consistent[1L]) {
      ## not consistent even to first level
      out <- x[0, , drop = FALSE]
    } else if (consistent == "all") {
      ## if require ALL consistent
      if (all(matches_consistent)) {
        max_ref_levels <- max_levels(ref_sub_)
        row <- which(max_ref_levels == max(max_ref_levels))[1L]
        out <- x[row, , drop = FALSE]
      } else {
        out <- x[0, , drop = FALSE]
      }
    } else {
      ## don't require ALL consistent and at least some are consistent

      # replace inconsistent values with NA
      by_i <- seq_along(by_ref)
      by_ref_inconsistent <- by_ref[by_i > max_matches_consistent]

      for (j in by_ref_inconsistent) {
        x[[j]] <- NA_character_
      }

      max_ref_levels <- max_levels(x, by = by_ref)

      if (consistent == "min") {
        row <- which(max_ref_levels == min(max_ref_levels))[1L]
      } else {
        row <- which(max_ref_levels == max(max_ref_levels))[1L]
      }

      out <- x[row, , drop = FALSE]
    }
  }
  out
}


#' @noRd
#' @importFrom dplyr bind_rows add_count
#' @importFrom rlang .data
resolve_join <- function(x, by_ref, temp_col_id, consistent = c("min", "max", "all")) {
  if (nrow(x) == 0L) {
    out <- x
  } else {
    consistent <- match.arg(consistent, choices = c("min", "max", "all"))

    # if only 1 row with temp_col_id, keep
    # if >=2 rows with temp_col_id, apply resolve_join_
    x <- dplyr::add_count(x, .data[[temp_col_id]], name = "N_TEMP_COL_ID")
    x_resolve_single <- x[x$N_TEMP_COL_ID == 1L, , drop = FALSE]

    if (any(x$N_TEMP_COL_ID >= 2L)) {
      x_multi <- x[x$N_TEMP_COL_ID >= 2L, , drop = FALSE]
      x_split <- split(x_multi, x_multi[[temp_col_id]])
      l_resolve <- lapply(x_split, resolve_join_, by_ref = by_ref, consistent = consistent)
      x_resolve_multi <- dplyr::bind_rows(l_resolve)
    } else {
      x_resolve_multi <- x[0L, , drop = FALSE]
    }

    out <- dplyr::bind_rows(x_resolve_single, x_resolve_multi)
    out$N_TEMP_COL_ID <- NULL
  }

  # return
  out
}


#' @noRd
resolve_join_ <- function(x, by_ref, consistent) {
  if (nrow(x) < 2L) {
    out <- x
  } else {
    ## 2 or more matches...
    ref_sub_ <- x[, by_ref, drop = FALSE]
    matches_consistent <- vapply(ref_sub_, unique_excl_na, FALSE)
    max_matches_consistent <- max_before_false(matches_consistent)

    if (!matches_consistent[1L]) {
      ## not consistent even to first level
      out <- x[0, , drop = FALSE]
    } else if (consistent == "all") {
      ## if require ALL consistent
      if (all(matches_consistent)) {
        max_ref_levels <- max_levels(ref_sub_, by = by_ref)
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

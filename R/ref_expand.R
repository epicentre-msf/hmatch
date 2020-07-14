#' Expand a reference data.frame containing N hierarchical columns to an N-level
#' reference data.frame
#'
#' @description
#' For example, a municipality-level reference data.frame might contain three
#' hierarchical columns — country, state, and municipality — but nonetheless
#' only reflect the municipality level in that all rows represent a unique
#' municipality. The lower-resolution levels (state, country) are implied but
#' not explicity represented as unique rows. If we wish to allow matches to the
#' lower-resolution levels, we need additional rows specific to these levels.
#'
#' This function takes a reference data.frame with N hierarchical columns, and
#' adds rows for each unique combination of each level that is not currently
#' explicitly represented.
#'
#' @param ref `data.frame` containing hierarchical columns with reference data
#' @param pattern regex pattern to match the names of the hierarchical columns
#'   in `ref` (supply either `pattern` *or* `by`)
#' @param by vector giving the names of the hierarchical columns in `ref`
#'   (supply either `pattern` *or* `by`)
#' @param lowest_level integer representing the lowest-resolution level
#'   (defaults to `1`)
#'
#' @return
#' A `data.frame` created by expanding `ref` to all implied hierarchical levels
#'
#' @examples
#' # subset example reference df to the admin-2 level
#' ne_ref_adm2 <- ne_ref[!is.na(ne_ref$adm2),]
#'
#' # expand back to all levels
#' ref_expand(ne_ref_adm2, pattern = "adm", lowest_level = 0)
#'
#' @export ref_expand
ref_expand <- function(ref, pattern, by, lowest_level = 1L) {


  ## match hierarchical columns
  by <- select_columns(ref, pattern, by)

  ## subset to hierarchical columns
  ref_ <- ref[, by, drop = FALSE]
  ref_bind <- NULL

  ## for each hierarchical level...
  for (i in seq_along(by)) {
    cols_focal <- by[1:i]

    rows_keep <- apply(
      ref_[,cols_focal, drop = FALSE],
      MARGIN = 1,
      FUN = function(x) !any(is.na(x))
    )

    ref_focal <- ref_[rows_keep, , drop = FALSE]
    ref_focal[setdiff(by, cols_focal)] <- NA_character_
    ref_focal$level <- lowest_level - 1L + i
    ref_focal <- unique(ref_focal)[c("level", by)]

    ref_bind <- rbind.data.frame(ref_bind, ref_focal)
  }

  ## remove rownames
  row.names(ref_bind) <- NULL

  ## return
  return(ref_bind)
}

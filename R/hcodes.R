#' Create codes to identify each unique combination of hierarchical levels in
#' a reference dataset
#'
#' Create codes to identify each unique combination of hierarchical levels in a
#' reference dataset. Codes may be integer-based (function `hcodes_int`) or
#' string-based (`hcodes_str`). Integer-based codes reflect the alphabetical
#' ranking of each level within the next-highest level. They are constant-width
#' and may optionally be prefixed with any given string. String-based codes are
#' created by pasting together the values of each hierarchical level with a
#' given separator (with options for string standardization prior to
#' collapsing).
#'
#' @param ref `data.frame` containing hierarchical columns with reference data
#' @param pattern regex pattern to match the names of the hierarchical columns
#'   in `ref` (supply either `pattern` *or* `by`)
#' @param by vector giving the names of the hierarchical columns in `ref`
#'   (supply either `pattern` *or* `by`)
#' @param prefix (only for `hcodes_int`) character prefix for integer-based
#'   codes (defaults to "")
#' @param sep (only for `hcodes_str`) desired separator between levels in
#'   string-based codes (defaults to "__")
#' @param std_fn (only for `hcodes_str`) Function to standardize input strings
#'   prior to creating codes. Defaults to \code{\link{string_std}}. Set to
#'   `NULL` to omit standardization. See also \link{string_standardization}.
#'
#' @return
#' A vector of codes
#'
#' @examples
#' data(ne_ref)
#'
#' # string-based codes
#' hcodes_str(ne_ref, pattern = "^adm")
#'
#' # integer-based codes
#' hcodes_int(ne_ref, pattern = "^adm")
#'
#' @name hcodes
NULL



#' @rdname hcodes
#' @export hcodes_str
hcodes_str <- function(ref,
                       pattern,
                       by,
                       sep = "__",
                       std_fn = string_std) {


  ## identify hierarchical columns
  by <- select_columns(ref, pattern, by, allow_both_null = FALSE)

  ## select hierarchical columns
  ref_ <- as.data.frame(ref)[, by, drop = FALSE]

  ## standardize values at each hierarchical level
  if (!is.null(std_fn)) {
    for (i in seq_len(ncol(ref_))) {
      ref_[[i]] <- std_fn(ref_[[i]])
    }
  }

  ## collapse standardize values from each level into hcode
  apply(ref_, 1, function(x) paste(x[!is.na(x)], collapse = sep))
}



#' @rdname hcodes
#' @export hcodes_int
hcodes_int <- function(ref,
                       pattern,
                       by,
                       prefix = "") {

  ## identify hierarchical columns
  by <- select_columns(ref, pattern, by, allow_both_null = FALSE)

  ## select hierarchical columns and convert to character
  ref_ <- as.data.frame(ref)[, by, drop = FALSE]
  ref_ <- as.data.frame(lapply(ref_, as.character))

  if (nrow(ref_) > 0) {

    ## integer ids for first column
    int_id_cols <- paste0("LEVEL_ID_", seq_along(by))
    ref_[[int_id_cols[1]]] <- integer_id(ref_[[1]])

    ## integer ids for all subsequent columns
    if (length(int_id_cols) > 1) {
      for (i in 2:length(by)) {
        col_focal <- int_id_cols[i]
        cols_split <- int_id_cols[1:(i-1)]
        col_focal_split <- split(ref_[[i]], ref_[cols_split])
        ref_[[col_focal]] <- unsplit(lapply(col_focal_split, integer_id), ref_[cols_split])
      }
    }

    ## select integer id columns
    int_ids <- ref_[,int_id_cols, drop = FALSE]
    int_ids <- as.data.frame(lapply(int_ids, as.integer))

    ## ensure integer ids constant-width at each level (pad with "0" if necessary)
    for(i in seq_along(int_id_cols)) {
      ids_col <- int_ids[[i]]
      if (length(ids_col) > 0) {
        n <- nchar(max(ids_col))
        int_ids[[i]] <- formatC(ids_col, width = n, flag = "0")
      }
    }

    ## collapse integer ids from each level into hcode
    out <- apply(int_ids, 1, function(x) paste0(prefix, paste0(x, collapse = "")))
  } else {
    out <- character(0)
  }

  out
}


#' @noRd
integer_id <- function(x) {
  out <- as.integer(as.factor(x))
  out[is.na(out)] <- 0L
  out
}


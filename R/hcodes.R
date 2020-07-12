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
#' hcodes_int(ne_ref, pattern = "^adm", prefix = "")
#'
#' @name hcodes
NULL



#' @rdname hcodes
#' @export hcodes_str
hcodes_str <- function(ref,
                       pattern = NULL,
                       by = NULL,
                       sep = "__",
                       std_fn = string_std) {

  if (is.null(pattern) & is.null(by)) {
    stop("Must provide either argument 'pattern' or argument 'by'")
  } else if (!is.null(pattern) & !is.null(by)) {
    warning("Arguments 'pattern' and 'by' are both non-NULL. ",
            "Ignoring 'pattern' and only using 'by'.")
  } else if (!is.null(pattern)) {
    by <- grep(pattern, names(ref), value = TRUE)
  }

  ref_ <- as.data.frame(ref)[, by, drop = FALSE]

  if (!is.null(std_fn)) {
    for (i in seq_len(ncol(ref_))) {
      ref_[[i]] <- std_fn(ref_[[i]])
    }
  }

  return(apply(ref_, 1, function(x) paste(x[!is.na(x)], collapse = sep)))
}


#' @rdname hcodes
#' @export hcodes_int
hcodes_int <- function(ref,
                       pattern = NULL,
                       by = NULL,
                       prefix = "") {

  if (is.null(pattern) & is.null(by)) {
    stop("Must provide either argument 'pattern' or argument 'by'")
  } else if (!is.null(pattern) & !is.null(by)) {
    warning("Arguments 'pattern' and 'by' are both non-NULL. ",
            "Ignoring 'pattern' and only using 'by'.")
  } else if (!is.null(pattern)) {
    by <- grep(pattern, names(ref), value = TRUE)
  }

  ref_ <- as.data.frame(ref)[, by, drop = FALSE]
  ref_ <- as.data.frame(lapply(ref_, as.character))

  ref_$LEVEL_ID_1 <- as.integer(as.factor(ref_[,1]))
  ref_$LEVEL_ID_1[is.na(ref_$LEVEL_ID_1)] <- 0L

  level_cols <- paste0("LEVEL_ID_", seq_along(by))

  if (length(level_cols) > 1) {
    for (i in 2:length(by)) {

      ii <- level_cols[1:(i-1)]
      col_focal <- level_cols[i]

      ref_split <- lapply(split(ref_, ref_[,ii]), function(df) {
        x <- as.integer(as.factor(df[,i]))
        x[is.na(x)] <- 0L
        df[[col_focal]] <- x
        df
      })

      ref_ <- unsplit(ref_split, ref_[,ii])
    }
  }

  levels_ <- ref_[,level_cols, drop = FALSE]

  for(i in 1:ncol(levels_)) {
    n <- nchar(max(levels_[,i]))
    levels_[,i] <- formatC(levels_[,i], width = n, flag = "0")
  }

  out <- apply(levels_, 1, function(x) paste0(prefix, paste0(x, collapse = "")))
  as.character(out)
}


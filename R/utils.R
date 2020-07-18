

order_within <- function(x, col, col_split) {
  split_factor <- factor(x[[col_split]], levels = unique(x[[col_split]]))
  x_split <- split(x, split_factor)
  x_split_order <- lapply(x_split, function(d) d[order(d[[col]]), , drop = FALSE])
  dplyr::bind_rows(x_split_order)
}


# pull_els <- function(x) {
#   lapply(x, function(x) x[[1]])
# }

# unnest_tokens <- function(x, col_id) {
#   x_split <- split(x, x[[col_id]])
#   x_unnest <- lapply(x_split, function(x) expand.grid(pull_els(x)))
#   dplyr::bind_rows(x_unnest)
# }


#' @importFrom tidyr unnest
#' @importFrom dplyr all_of
unnest_tokens <- function(x, by) {
  for (j in by) { x <- tidyr::unnest(x, all_of(j)) }
  x
}


tokenize <- function(x, split = "[-_[:space:]]+") {
  strsplit(x, split)
}


tokenize_cols <- function(x, by, split, prefix = "token_") {
  x <- dplyr::as_tibble(x)
  bind_ <- x[, by, drop = FALSE]
  for (j in by) { bind_[[j]] <- tokenize(bind_[[j]], split = split) }
  names(bind_) <- paste0(prefix, by)
  dplyr::bind_cols(x, bind_)
}



#' @noRd
backtick <- function(x) {
  paste0("`", x, "`")
}



#' Turn vector into dput-like output, for use in warnings and errors
#' @noRd
vec_paste_c <- function(x) {
  len_x <- length(x)
  x[!is.na(x)] <- dQuote(x[!is.na(x)], q = FALSE)
  x <- paste(x, collapse = ", ")
  if (len_x > 1) {
    x <- paste0("c(", x, ")")
  }
  x
}


#' @noRd
set_names <- function(object = nm, nm) {
  names(object) <- nm
  object
}


#' @noRd
n_levels <- function(x, pattern = NULL, by = NULL) {
  by <- select_columns(x, pattern, by)
  m <- !is.na(x[, by, drop = FALSE])
  as.integer(apply(m, 1, sum))
}


#' @noRd
complete_sequence <- function(x, by) {
  n_levels(x, by = by) == max_levels(x, by = by)
}



#' @noRd
unique_excl_na <- function(x) {
  all(is.na(x)) | length(unique(x[!is.na(x)])) == 1L
}



#' @noRd
#' @importFrom dplyr recode
#' @importFrom rlang `!!!`
#' @importFrom stats setNames
apply_dict <- function(x, dict, by_raw, by_join, std_fn) {

  if (ncol(dict) == 2) { dict[[3]] <- NA_character_ }
  if (is.null(std_fn)) { std_fn <- as.character }

  dict[[1]] <- std_fn(dict[[1]])
  dict[[2]] <- std_fn(dict[[2]])

  for (j in seq_along(by_raw)) {

    by_raw_j <- by_raw[j]
    by_join_j <- by_join[j]

    dict_j <- dict[dict[[3]] == by_raw_j | is.na(dict[[3]]),]

    if (nrow(dict_j) > 0) {
      vals_recode <- setNames(dict_j[[2]], dict_j[[1]])
      x[[by_join_j]] <- dplyr::recode(x[[by_join_j]], !!!vals_recode)
    }
  }
  return(x)
}


#' @noRd
add_column <- function(dat, colname, values) {
  dat[[colname]] <- if (nrow(dat) > 0) {
    values
  } else {
    vector(mode = class(values), length = 0)
  }
  dat
}


#' @noRd
rbind_dfs <- function(x, y) {
  cols_add_to_x <- setdiff(names(y), names(x))
  cols_add_to_y <- setdiff(names(x), names(y))
  for (j in cols_add_to_x) { x <- add_column(x, j, NA) }
  for (j in cols_add_to_y) { y <- add_column(y, j, NA) }
  rbind.data.frame(x, y)
}


#' @noRd
add_join_columns <- function(dat, by, join_cols, std_fn = NULL, ...) {

  bind_ <- dat[, by, drop = FALSE]
  if (!is.null(std_fn)) {
    for (j in seq_len(ncol(bind_))) {
      bind_[[j]] <- std_fn(bind_[[j]], ...)
    }
  }
  names(bind_) <- join_cols
  cbind(dat, bind_)
}



#' @noRd
corresponding_levels <- function(dat, by_raw, by_ref) {
  max_level_raw <- max_levels(dat, by = by_raw)
  max_level_ref <- max_levels(dat, by = by_ref)
  dat[max_level_raw >= max_level_ref & max_level_ref > 0,]
}



#' @noRd
max_before_false <- function(x) {
  if (!x[1]) {
    return(NA_character_)
  } else if (any(!x)) {
    return(min(which(!x)) - 1)
  } else {
    return(length(x))
  }
}



#' @importFrom dplyr arrange group_by_all ungroup
split_raw <- function(raw, by, lev, all_levels = TRUE) {

  out <- unique(raw[, by[1:lev], drop = FALSE])

  all_na <- apply(out, 1, function(x) all(is.na(x)))
  out <- out[!all_na, , drop = FALSE]

  out <- out[!is.na(out[[by[lev]]]), , drop = FALSE]

  if (all_levels) {
    out[,setdiff(by, names(out))] <- NA_character_
  }

  out_order <- order(hcodes_int(out, by = by[1:lev]))
  out[out_order, , drop = FALSE]
}


split_ref <- function(ref, by, lev, lower_levels = FALSE) {

  l <- max_levels(ref, by = by)

  out <- if (lower_levels) {
    ref[l <= lev,]
  } else {
    ref[l == lev,]
  }

  # if (lev < length(by)) {
  #   cols_excl <- by[(lev + 1):length(by)]
  #   out <- out[,!names(out) %in% cols_excl, drop = FALSE]
  # }
  return(out)
}



#' @noRd
ordered_split <- function(x, f, N) {
  s <- split(x, f)
  i <- as.numeric(names(s))
  out <- vector("list", N)
  out[i] <- s
  return(out)
}


spmatch_prep_levels <- function(levels, by) {

  if (is.null(levels)) {
    valid <- TRUE
    out <- seq_along(by)
  } else if (is.numeric(levels)) {
    valid <- levels %in% seq_along(by)
    out <- levels
  } else {
    valid <- levels %in% by
    out <- match(levels, by)
  }

  if (!all(valid)) {
    stop("the following elements of `levels` could not be matched to a ",
         "hierarchical column within `raw`: ",
         paste(levels[!valid], collapse = "; "))
  }
  return(out)
}


#' @importFrom stats setNames
spmatch_prep <- function(raw,
                         ref,
                         pattern,
                         pattern_ref,
                         by,
                         by_ref = by_ref,
                         ref_prefix,
                         levels,
                         lower_levels = FALSE) {

  prep <- prep_match_columns(
    raw = raw,
    ref = ref,
    pattern = pattern,
    pattern_ref = pattern_ref,
    by = by,
    by_ref = by_ref,
    ref_prefix = ref_prefix
  )

  levels <- spmatch_prep_levels(levels, prep$by_raw)

  raw_split <- lapply(
    seq_along(prep$by_raw)[levels],
    split_raw,
    raw = raw,
    by = prep$by_raw
  )

  ref_split <- lapply(
    seq_along(prep$by_ref)[levels],
    split_ref,
    ref = ref,
    by = prep$by_ref_orig,
    lower_levels = lower_levels
  )

  return(list(raw_split = raw_split,
              ref_split = ref_split,
              by_raw_split = prep$by_raw,
              by_ref_split = prep$by_ref_orig,
              names = prep$by_raw[levels]))
}

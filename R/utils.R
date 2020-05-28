

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
rename_col <- function(dat, col_old, col_new) {
  names(dat)[names(dat) == col_old] <- col_new
  dat
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
test_geocode_conflict <- function(data, pattern) {
  code_columns <- sort(grep(pattern, names(data), value = TRUE))
  data <- data[, code_columns, drop = FALSE]
  apply(data, 1, test_geocode_conflict_row)
}


#' @noRd
test_geocode_conflict_row <- function(x) {

  x <- x[!is.na(x)]
  n <- length(x)
  conflict <- FALSE

  if (n > 1) {
    for (i in seq_len(n-1)) {
      if (!grepl(x[i], x[i+1])) {
        conflict <- TRUE
      }
    }
  }
  conflict
}


#' @noRd
corresponding_levels <- function(dat, by_raw, by_ref) {
  max_level_raw <- apply(dat[, by_raw, drop = FALSE], 1, max_not_na, no_na = -1L)
  max_level_ref <- apply(dat[, by_ref, drop = FALSE], 1, max_not_na, no_na = 0L)
  dat[max_level_raw >= max_level_ref,]
}


#' @noRd
max_not_na <- function(x, no_na = 0L, value = FALSE) {
  ifelse(any(!is.na(x)), max(which(!is.na(x))), no_na)
}


#' @noRd
max_not_na_value <- function(x) {
  ifelse(any(!is.na(x)), x[max(which(!is.na(x)))], NA_character_)
}


#' @noRd
max_adm_level <- function(dat, by = NULL, no_na = 0L) {
  by <- if (is.null(by)) { names(dat) } else { by }
  apply(dat[, by, drop = FALSE], 1, max_not_na, no_na = no_na)
}


#' @noRd
best_geocode <- function(dat, pattern) {
  cols <- grep(pattern, names(dat), value = TRUE)
  apply(dat[, sort(cols), drop = FALSE], 1, max_not_na_value)
}


#' @noRd
sort_cols <- function(dat, ref) {
  cols_1 <- intersect(names(ref), names(dat))
  cols_2 <- setdiff(names(dat), names(ref))
  dat[, c(cols_1, cols_2), drop = FALSE]
}


#' @noRd
prep_match_columns <- function(raw,
                               ref,
                               pattern_raw,
                               pattern_ref,
                               by,
                               ref_prefix = "ref_",
                               join_suffix = "___JOIN_",
                               code_col = NULL) {

  if (!is.null(pattern_raw)) {
    by_raw <- grep(pattern_raw, names(raw), value = TRUE)
    by_ref <- grep(pattern_ref, names(ref), value = TRUE)
  } else if (!is.null(by)) {
    by_raw <- names(by)
    by_ref <- by
  } else {
    by_raw <- intersect(names(ref), names(raw))
    by_ref <- intersect(names(ref), names(raw))
  }

  by_ref_orig <- by_ref

  # rename cols of ref if necessary
  if (all(by_raw == by_ref)) {
    by_ref_i <- vapply(by_ref, function(x) which(names(ref) == x), 0L)
    by_ref <- paste0(ref_prefix, by_ref)
    names(ref)[by_ref_i] <- by_ref
  }

  by_join <- paste0(by_raw, join_suffix)

  if (!is.null(code_col)) {
    ref[[code_col]] <- hcodes_str(ref, by = by_ref)
  }

  return(list(ref = ref, by_raw = by_raw, by_ref = by_ref, by_ref_orig = by_ref_orig, by_join = by_join))
}


#' @noRd
best_geocode_group <- function(dat, pattern, id_col, code_col, split = "__") {
  cols <- grep(pattern, names(dat), value = TRUE)
  dat_ <- dat[,sort(cols), drop = FALSE]
  dat_split <- split(dat_, dat[[id_col]])
  vapply(dat_split,
         best_geocode_helper,
         character(1),
         code_col = code_col,
         USE.NAMES = FALSE)
}


#' @noRd
best_geocode_helper <- function(dat, pattern, code_col, id_col, split = "__") {

  # TODO: make this code less awful
  cols <- grep(pattern, names(dat), value = TRUE)
  dat_ <- dat[, sort(cols), drop = FALSE]
  id <- unique(dat[[id_col]])

  n_levels <- ncol(dat_)

  geocodes <- unlist(dat_, use.names = FALSE)
  geocodes <- unique(geocodes[!is.na(geocodes)])

  zz <- lapply(geocodes, function(x) fill_vec_na(strsplit(x, split)[[1]], N = n_levels))
  yy <- do.call(rbind, zz)
  xx <- apply(yy, 2, function(x) length(unique(x[!is.na(x)])) == 1L)
  uu <- max_before_false(xx)

  if (!is.na(uu)) {
    tt <- 1:uu
    yy <- if (length(tt) == 1) cbind(yy[,1:uu, drop = FALSE]) else yy[, 1:uu, drop = FALSE]
    ss <- apply(yy, 2, function(x) unique(x[!is.na(x)]))
    code <- paste(ss, collapse = split)
  } else {
    code <- NA_character_
  }
  return(setNames(data.frame(id, code, stringsAsFactors = FALSE), c(id_col, code_col)))
}



#' @noRd
fill_vec_na <- function(x, N) {
  d <- N - length(x)
  if (d > 0) c(x, rep(NA_character_, d)) else x
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
split_raw <- function(raw, by, lev) {
  out <- unique(raw[, by[1:lev], drop = FALSE])

  all_na <- apply(out, 1, function(x) all(is.na(x)))
  out <- out[!all_na, , drop = FALSE]

  out <- out[!is.na(out[[by[lev]]]), , drop = FALSE]

  # arrange by col
  out_grp <- dplyr::group_by_all(out)
  out_arn <- dplyr::arrange(out_grp, .by_group = TRUE)
  out <- ungroup(out_arn)
  class(out) <- class(raw)
  out
}


split_ref <- function(ref, by, lev, lower_levels = FALSE) {

  l <- max_adm_level(ref, by = by)

  out <- if (lower_levels) {
    ref[l <= lev,]
  } else {
    ref[l == lev,]
  }

  if (lev < length(by)) {
    cols_excl <- by[(lev + 1):length(by)]
    out <- out[,!names(out) %in% cols_excl, drop = FALSE]
  }
  return(out)
}


#' @importFrom stats setNames
split_by <- function(lev, by_raw, by_ref) {
  setNames(by_raw[1:lev], by_ref[1:lev])
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


spmatch_prep <- function(raw,
                         ref,
                         pattern_raw,
                         pattern_ref,
                         by,
                         ref_prefix,
                         levels,
                         lower_levels = FALSE) {

  prep <- prep_match_columns(raw = raw,
                             ref = ref,
                             pattern_raw = pattern_raw,
                             pattern_ref = pattern_ref,
                             by = by,
                             ref_prefix = ref_prefix)

  levels <- spmatch_prep_levels(levels, prep$by_raw)

  raw_split <- lapply(seq_along(prep$by_raw)[levels],
                      split_raw,
                      raw = raw,
                      by = prep$by_raw)

  ref_split <- lapply(seq_along(prep$by_ref)[levels],
                      split_ref,
                      ref = ref,
                      by = prep$by_ref_orig,
                      lower_levels = lower_levels)

  by_split <- lapply(seq_along(prep$by_raw)[levels],
                     split_by,
                     by_raw = prep$by_raw,
                     by_ref = prep$by_ref_orig)

  return(list(raw_split = raw_split,
              ref_split = ref_split,
              by_split = by_split,
              names = prep$by_raw[levels]))
}


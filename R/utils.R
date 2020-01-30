

#' @noRd
#' @importFrom dplyr mutate_all bind_cols
add_join_columns <- function(dat, by, join_cols, std_fn = NULL) {
  bind_ <- dat[, by, drop = FALSE]
  if (!is.null(std_fn)) {
    bind_ <- dplyr::mutate_all(bind_, std_fn)
  }
  names(bind_) <- join_cols
  dplyr::bind_cols(dat, bind_)
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
prep_ref <- function(raw, ref, pattern_raw, pattern_ref, by, join_suffix = "___JOIN_") {

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

  # rename cols of ref if necessary
  if (all(by_raw == by_ref)) {
    by_ref_i <- vapply(by_ref, function(x) which(names(ref) == x), 0L)
    by_ref <- paste0("bind_", by_ref)
    names(ref)[by_ref_i] <- by_ref
  }

  by_join <- paste0(by_raw, join_suffix)

  return(list(ref = ref, by_raw = by_raw, by_ref = by_ref, by_join = by_join))
}


#' @noRd
best_geocode_group <- function(dat, pattern, id_col, split = "__") {
  cols <- grep(pattern, names(dat), value = TRUE)
  dat_ <- dat[,sort(cols), drop = FALSE]
  dat_split <- split(dat_, dat[[id_col]])
  vapply(dat_split, best_geocode_helper, character(1), USE.NAMES = FALSE)
}


#' @noRd
best_geocode_helper <- function(dat, pattern, split = "__") {

  cols <- grep(pattern, names(dat), value = TRUE)
  dat_ <- dat[, sort(cols), drop = FALSE]

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
  return(data.frame(pcode = code, stringsAsFactors = FALSE))
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


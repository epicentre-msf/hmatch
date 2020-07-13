
#' @noRd
set_names <- function(object = nm, nm) {
  names(object) <- nm
  object
}


#' @noRd
n_levels <- function(x,
                     pattern = NULL,
                     by = NULL,
                     sort = FALSE) {

  if (!is.null(pattern) & !is.null(by)) {
    stop("only one of `pattern` or `by` should be provided")
  } else if (is.null(by) & is.null(pattern)) {
    by <- names(x)
  } else if (!is.null(pattern)) {
    by <- grep(pattern, names(x), value = TRUE)
  }

  if (sort) by <- sort(by)

  m <- !is.na(x[, by, drop = FALSE])
  apply(m, 1, sum)
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


#' #' @noRd
#' max_not_na <- function(x, no_na = 0L, value = FALSE) {
#'   ifelse(any(!is.na(x)), max(which(!is.na(x))), no_na)
#' }


#' @noRd
max_not_na_value <- function(x) {
  ifelse(any(!is.na(x)), x[max(which(!is.na(x)))], NA_character_)
}


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


  if (!is.null(pattern)) {
    by_raw <- grep(pattern, names(raw), value = TRUE)
    by_ref <- grep(pattern_ref, names(ref), value = TRUE)
  } else if (!is.null(by)) {
    by_raw <- by
    by_ref <- by_ref
  } else {
    by_raw <- intersect(names(ref), names(raw))
    by_ref <- intersect(names(ref), names(raw))
  }

  by_ref_orig <- by_ref

  # rename cols of ref if necessary
  if (all(by_raw == by_ref)) {
    by_ref <- paste0(ref_prefix, by_ref)
    names(ref)[match(by_raw, names(ref))] <- by_ref
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


#' Hierarchical matching, separately at each hierarchical level
#'
#' @description
#' Implements hierarchical matching, separately at each hierarchical level
#' within the data. For a given level, the raw data that is matched includes
#' every unique combination of values at and below the level of interest. E.g.
#'
#' Level 1: \cr
#' `| Canada        |` \cr
#' `| United States |` \cr
#'
#' Level 2: \cr
#' `| Canada        | Ontario      |` \cr
#' `| United States | New York     |` \cr
#' `| United States | Pennsylvania |` \cr
#'
#' Level 3: \cr
#' `| Canada        | Ontario      | Ottawa       |` \cr
#' `| Canada        | Ontario      | Toronto      |` \cr
#' `| United States | New York     | Bronx        |` \cr
#' `| United States | New York     | New York     |` \cr
#' `| United States | Pennsylvania | Philadelphia |`
#'
#' @inheritParams hmatch_tokens
#' @inheritParams hmatch_composite
#'
#' @param fn which function to use for matching. Current options are
#'   \code{\link{hmatch}}, \code{\link{hmatch_permute}},
#'   \code{\link{hmatch_tokens}}, \code{\link{hmatch_settle}}, or
#'   \code{\link{hmatch_composite}}. Defaults to "hmatch".
#'
#'   Note that some subsequent arguments are only relevant for specific
#'   functions (e.g. the `exclude_` arguments are only relevant if `fn =
#'   "hmatch_tokens"`).
#' @param type type of join ("left", "inner", "anti", "resolve_left",
#'   "resolve_inner", or "resolve_anti"). Defaults to "left". See
#'   \link{join_types}.
#'
#'   Note that the details of resolve joins vary somewhat among hmatch functions
#'   (see documentation for the relevant function), and that function
#'   \code{\link{hmatch_composite}} only allows resolve joins.
#' @param levels a vector of names or integer indices corresponding to one or
#'   more of the hierarchical columns in `raw` to match at. Defaults to `NULL`
#'   in which case matches are made at each hierarchical level.
#' @param always_list logical indicating whether to always return a list, even
#'   when argument `levels` specifies a single match level. Defaults to `FALSE`.
#'
#' @return
#' A list of data frames, each returned by a call to `fn` on the unique
#' combination of hierarchical values at the given hierarchical level. The
#' number of elements in the list corresponds to the number of hierarchical
#' columns in `raw`, or, if specified, the number of elements in argument
#' `levels`.
#'
#' However, if `always_list = FALSE` and `length(levels) == 1`, a single data
#' frame is returned (i.e. not wrapped in a list).
#'
#' @examples
#' data(ne_raw)
#' data(ne_ref)
#'
#' # by default calls fn `hmatch` separately for each hierarchical level
#' hmatch_split(ne_raw, ne_ref)
#'
#' # can also specify other hmatch functions, and subsets of hierarchical levels
#' hmatch_split(ne_raw, ne_ref, fn = "hmatch_tokens", levels = 2:3)
#'
#' @export hmatch_split
hmatch_split <- function(raw,
                         ref,
                         pattern,
                         pattern_ref = pattern,
                         by,
                         by_ref = by,
                         fn = "hmatch",
                         type = "left",
                         allow_gaps = TRUE,
                         fuzzy = FALSE,
                         fuzzy_method = "osa",
                         fuzzy_dist = 1L,
                         dict = NULL,
                         ref_prefix = "ref_",
                         std_fn = string_std,
                         ...,
                         levels = NULL,
                         always_list = FALSE,
                         man,
                         code_col,
                         always_tokenize = FALSE,
                         token_split = "_",
                         exclude_freq = 3,
                         exclude_nchar = 3,
                         exclude_values = NULL) {

  ## validate arg fn
  fn_name <- as.character(substitute(fn))
  fn <- match.fun(fn)

  ## identify hierarchical columns and split raw and ref by hierarchical level
  prep <- hmatch_split_prep(
    raw = raw,
    ref = ref,
    pattern = pattern,
    pattern_ref = pattern_ref,
    by = by,
    by_ref = by_ref,
    ref_prefix = ref_prefix,
    levels = levels,
    lower_levels = FALSE
  )

  ## implement matching routines, split by hierarchical level
  if (fn_name %in% c("hmatch", "hmatch_settle", "hmatch_permute")) {

    out <- mapply(
      fn,
      raw = prep$raw_split,
      ref = prep$ref_split,
      MoreArgs = list(
        by = prep$by_raw_split,
        by_ref = prep$by_ref_split,
        type = type,
        allow_gaps = allow_gaps,
        fuzzy = fuzzy,
        fuzzy_method = fuzzy_method,
        fuzzy_dist = fuzzy_dist,
        dict = dict,
        ref_prefix = ref_prefix,
        std_fn = std_fn,
        ...
      ),
      SIMPLIFY = FALSE
    )

  } else if (fn_name == "hmatch_composite") {

    if (missing(man)) man <- NULL
    if (missing(code_col)) code_col <- NULL

    if (type %in% c("left", "inner", "anti")) {
      type <- paste0("resolve_", type)
      warning(
        "`hmatch_composite` only implements resolve joins. Updating argument ",
        "`type` to ", vec_paste_c(type), call. = FALSE
      )
    }

    out <- mapply(
      fn,
      raw = prep$raw_split,
      ref = prep$ref_split,
      MoreArgs = list(
        by = prep$by_raw_split,
        by_ref = prep$by_ref_split,
        type = type,
        allow_gaps = allow_gaps,
        fuzzy = fuzzy,
        fuzzy_method = fuzzy_method,
        fuzzy_dist = fuzzy_dist,
        dict = dict,
        ref_prefix = ref_prefix,
        std_fn = std_fn,
        ...,
        man = man,  # TODO: probably need to split by level
        code_col = code_col
      ),
      SIMPLIFY = FALSE
    )
  } else if (fn_name == "hmatch_tokens") {

    out <- mapply(
      fn,
      raw = prep$raw_split,
      ref = prep$ref_split,
      MoreArgs = list(
        by = prep$by_raw_split,
        by_ref = prep$by_ref_split,
        type = type,
        allow_gaps = allow_gaps,
        fuzzy = fuzzy,
        fuzzy_method = fuzzy_method,
        fuzzy_dist = fuzzy_dist,
        dict = dict,
        ref_prefix = ref_prefix,
        std_fn = std_fn,
        ...,
        always_tokenize = always_tokenize,
        token_split = token_split,
        exclude_freq = exclude_freq,
        exclude_nchar = exclude_nchar,
        exclude_values = exclude_values
      ),
      SIMPLIFY = FALSE
    )

  }

  names(out) <- prep$names

  if (length(out) == 1L & !always_list) out <- out[[1]]

  return(out)
}



#' @importFrom dplyr arrange group_by_all ungroup
split_raw <- function(raw, by, lev, all_levels = TRUE) {

  out <- unique(raw[, by[1:lev], drop = FALSE])

  all_na <- apply(out, 1, function(x) all(is.na(x)))
  out <- out[!all_na, , drop = FALSE]

  out <- out[!is.na(out[[by[lev]]]), , drop = FALSE]

  if (all_levels) {
    cols_missing <- setdiff(by, names(out))
    for (j in cols_missing) {
      out[[j]] <- rep(NA_character_, nrow(out))
    }
  }

  out_order <- order(hcodes_int(out, by = by[1:lev]))
  out[out_order, , drop = FALSE]
}


split_ref <- function(ref, by, lev, lower_levels = FALSE) {

  l <- max_levels(ref, by = by)

  out <- if (lower_levels) {
    ref[l <= lev, , drop = FALSE]
  } else {
    ref[l == lev, , drop = FALSE]
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


hmatch_split_prep_levels <- function(levels, by) {

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
hmatch_split_prep <- function(raw,
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

  levels <- hmatch_split_prep_levels(levels, prep$by_raw)

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


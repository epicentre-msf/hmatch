#' Hierarchical matching, separately at each hierarchical level
#'
#' @description
#' Implements hierarchical matching with \code{\link{hmatch}}, separately at
#' each hierarchical level within the data. For a given level, the raw data that
#' is matched includes every unique combination of values at and below the level
#' of interest. E.g.
#'
#' L1: Canada \cr
#' L1: United States \cr
#'
#' L2: Canada | Ontario \cr
#' L2: United States | New York \cr
#' L2: United States | Pennsylvania \cr
#'
#' L3: Canada | Ontario | Ottawa \cr
#' L3: Canada | Ontario | Toronto \cr
#' L3: United States | New York | New York \cr
#' L3: United States | Pennsylvania | Philadelphia
#'
#' @inheritParams hmatch
#'
#' @param levels a vector of names or integer indices corresponding to one or
#'   more of the hierarchical columns in `raw` to match at. Defaults to `NULL`
#'   in which case matches are made at each hierarchical level.
#' @param always_list logical indicating whether to always return a list, even
#'   when argument `levels` specifies a single match level (defaults to `FALSE`)
#'
#' @return
#' A list of data frames, each returned by a call to `hmatch` on the
#' unique combination of hierarchical values at the given hierarchical level.
#' The number of elements in the list corresponds to the number of hierarchical
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
#' # find all non-matches ("anti"-join) at each hierarchical level
#' spmatch(ne_raw, ne_ref, type = "anti")
#'
#' # find all matches ("inner"-join) at only the adm2 level
#' spmatch(ne_raw, ne_ref, type = "inner", levels = "adm2")
#'
#' # with dictionary-based recoding
#' ne_dict <- data.frame(value = "USA",
#'                       replacement = "United States",
#'                       variable = "adm0")
#'
#' spmatch(ne_raw, ne_ref, type = "inner", dict = ne_dict, levels = "adm2")
#'
#' @export spmatch
spmatch <- function(raw,
                    ref,
                    pattern = NULL,
                    pattern_ref = pattern,
                    by = NULL,
                    by_ref = by_ref,
                    dict = NULL,
                    type = "left",
                    ref_prefix = "ref_",
                    fuzzy = FALSE,
                    fuzzy_method = "osa",
                    fuzzy_dist = 1L,
                    std_fn = string_std,
                    ...,
                    levels = NULL,
                    always_list = FALSE) {

  # # for testing
  # raw = ne_raw
  # ref = ne_ref
  # pattern = NULL
  # pattern_ref = pattern
  # by = NULL
  # dict <- NULL
  # type = "inner"
  # ref_prefix = "ref_"
  # fuzzy = FALSE
  # fuzzy_dist = 1L
  # std_fn = string_std
  # ... <- NULL
  # levels <- NULL

  prep <- spmatch_prep(
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

  out <- mapply(
    hmatch,
    raw = prep$raw_split,
    ref = prep$ref_split,
    MoreArgs = list(
      by = prep$by_raw_split,
      by_ref = prep$by_ref_split,
      type = type,
      dict = dict,
      ref_prefix = ref_prefix,
      fuzzy = fuzzy,
      fuzzy_method = fuzzy_method,
      fuzzy_dist = fuzzy_dist,
      std_fn = std_fn,
      ...
    ),
    SIMPLIFY = FALSE
  )

  names(out) <- prep$names

  if (length(levels) == 1L & !always_list) out <- out[[1]]

  return(out)
}


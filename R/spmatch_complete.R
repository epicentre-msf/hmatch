#' Complete hierarchical matching, separately at each hierarchical level
#'
#' @description
#' Implements hierarchical matching with \code{\link{hmatch_complete}},
#' separately at each hierarchical level within the data. For a given level, the
#' raw data that is matched includes every unique combination of values at and
#' below the level of interest. E.g.
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
#' @inheritParams hmatch_complete
#'
#' @param levels a vector of names or integer indices corresponding to one or
#'   more of the hierarchical columns in `raw` to match at. Defaults to `NULL`
#'   in which case matches are made at each hierarchical level.
#' @param always_list logical indicating whether to always return a list, even
#'   when argument `levels`` specifies a single match level (defaults to
#'   `FALSE`)
#'
#' @return
#' A list of data frames, each returned by a call to `hmatch_complete` on the
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
#' spmatch_complete(ne_raw, ne_ref, type = "left")
#'
#' # find all matches ("inner"-join) at only the adm0 level
#' spmatch_complete(ne_raw, ne_ref, type = "inner", levels = "adm0")
#'
#' # with dictionary-based recoding
#' ne_dict <- data.frame(value = "USA",
#'                       replacement = "United States",
#'                       variable = "adm0")
#'
#' spmatch_complete(ne_raw, ne_ref, type = "inner", dict = ne_dict, levels = "adm0")
#'
#' @export spmatch_complete
spmatch_complete <- function(raw,
                             ref,
                             pattern_raw = NULL,
                             pattern_ref = pattern_raw,
                             by = NULL,
                             dict = NULL,
                             type = "left",
                             ref_prefix = "ref_",
                             std_fn = string_std,
                             ...,
                             levels = NULL,
                             always_list = FALSE) {

  # # for testing
  # raw = ne_raw
  # ref = ne_ref
  # pattern_raw = NULL
  # pattern_ref = pattern_raw
  # by = NULL
  # dict <- NULL
  # type = "anti"
  # ref_prefix = "ref_"
  # std_fn = string_std
  # ... <- NULL
  # levels <- NULL

  prep <- spmatch_prep(
    raw = raw,
    ref = ref,
    pattern_raw = pattern_raw,
    pattern_ref = pattern_ref,
    by = by,
    ref_prefix = ref_prefix,
    levels = levels
  )

  out <- mapply(
    hmatch_complete,
    raw = prep$raw_split,
    ref = prep$ref_split,
    MoreArgs = list(
      by = prep$by_split,
      dict = dict,
      type = type,
      ref_prefix = ref_prefix,
      std_fn = std_fn,
      ...
    ),
    SIMPLIFY = FALSE
  )

  names(out) <- prep$names

  if (length(levels) == 1L & !always_list) out <- out[[1]]

  return(out)
}


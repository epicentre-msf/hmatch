#' Implement a variety of hierarchical matching strategies in sequence,
#' separately at each hierarchical level
#'
#' @description
#' Implements hierarchical matching with \code{\link{hmatch_composite}},
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
#' @inheritParams hmatch_composite
#' @inheritParams spmatch
#'
#' @return
#' A list of data frames, each returned by a call to `hmatch_composite` on the
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
#' spmatch_composite(ne_raw, ne_ref, type = "resolve_anti", fuzzy = TRUE)
#'
#' # find all matches ("inner"-join) at only the adm2 level
#' spmatch_composite(ne_raw, ne_ref, type = "resolve_inner", levels = "adm2")
#'
#' # with dictionary-based recoding
#' ne_dict <- data.frame(value = "USA",
#'                       replacement = "United States",
#'                       variable = "adm0")
#'
#' spmatch_composite(ne_raw, ne_ref, type = "resolve_inner", dict = ne_dict, levels = "adm2")
#'
#' @export spmatch_composite
spmatch_composite <- function(raw,
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
  # type = "anti"
  # ref_prefix = "ref_"
  # fuzzy = TRUE
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
    lower_levels = TRUE
  )

  out <- mapply(
    hmatch_composite,
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
      # concise = concise,
      std_fn = std_fn,
      ...
    ),
    SIMPLIFY = FALSE
  )

  names(out) <- prep$names

  if (length(levels) == 1L & !always_list) out <- out[[1]]

  return(out)
}


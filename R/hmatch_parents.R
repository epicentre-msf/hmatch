#' Hierarchichal matching of parents based on sets of common offspring
#'
#' Match a data.frame with raw, potentially messy hierarchical data (e.g.
#' province, county, township) against a reference dataset, using partial
#' matching. "Partial" here means that one or more hierarchical levels within
#' the raw data may be missing (i.e. NA). More specifically, for a given row of
#' raw data, matches can potentially be made to a high-resolution level (e.g.
#' township) even if one or more lower-resolution levels (e.g. province) is
#' missing.
#'
#' @inheritParams hmatch_partial
#'
#' @param x a `data.frame` representing one or more rows from `raw`, and
#'   optionally including matching columns from `ref` to help narrow down the
#'   set of possible offspring to try matching
#' @param level integer index of the hierarchical level to match at
#' @param mmin minimum number of matching offspring required for parents to be
#'   considered a match
#' @param pmin minimum proportion of matching offspring required for parents to
#'   be considered a match (reflects the proportion of offspring in `raw` with
#'   match in `ref`)
#' @param type type of join ("inner" or "left") (defaults to "left")
#'
#' @return a data frame obtained by matching the hierarchical columns in `raw`
#'   and `ref`, using the join type specified by argument `type` (see
#'   \link{join_types} for more details)
#'
#' @examples
#' data(ne_raw)
#' data(ne_ref)
#'
#' hmatch_parents(ne_raw,
#'                ne_raw,
#'                ne_ref,
#'                pattern = "adm",
#'                level = 1,
#'                mmin = 1,
#'                pmin = 0.5)
#'
#' @importFrom dplyr left_join bind_rows
#' @export hmatch_parents
hmatch_parents <- function(x,
                           raw,
                           ref,
                           level,
                           mmin,
                           pmin,
                           pattern = NULL,
                           pattern_ref = pattern,
                           by = NULL,
                           by_ref = by,
                           type = "left",
                           ref_prefix = "ref_",
                           fuzzy = FALSE,
                           max_dist = 1L,
                           std_fn = string_std,
                           ...) {

  # pattern = "adm"
  # pattern_ref = pattern
  # level <- 2
  # by = NULL
  # dict <- NULL
  # type = "left"
  # ref_prefix = "ref_"
  # std_fn = string_std
  # max_dist <- 1
  # mmin = 2
  # pmin = 0.5
  # ... <- NULL

  x$TEMP_ROW_ID <- seq_len(nrow(x))

  ## identify hierarchical columns to match, and rename ref cols if necessary
  prep <- prep_match_columns(raw = raw,
                             ref = ref,
                             pattern = pattern,
                             pattern_ref = pattern_ref,
                             by = by,
                             by_ref = by_ref,
                             ref_prefix = ref_prefix)

  # add any missing `ref` names to x
  ref_names_missing <- setdiff(prep$by_ref, names(x))
  x[ref_names_missing] <- NA_character_

  raw_ <- unique(raw[,prep$by_raw])
  ref_ <- prep$ref[,prep$by_ref]

  raw_lev <- max_levels(raw_, by = prep$by_raw)
  ref_lev <- max_levels(ref_, by = prep$by_ref)

  raw_split <- ordered_split(raw_, raw_lev, N = length(prep$by_raw))
  ref_split <- ordered_split(ref_, ref_lev, N = length(prep$by_raw))

  ref_prep_split <- split(prep$ref, ref_lev)

  xraw <- x[,c("TEMP_ROW_ID", prep$by_raw[1:level])]
  xref <- x[,c("TEMP_ROW_ID", prep$by_ref[1:(level-1)])]

  child_raw <- prep$by_raw[level+1]
  child_ref <- prep$by_ref[level+1]

  xraw_join <- dplyr::left_join(xraw, raw_split[[level+1]], by = prep$by_raw[1:level])
  xref_join <- prep_xref(xref, ref_split[[level+1]], by = prep$by_ref[1:(level-1)])

  xraw_join[[child_raw]] <- std_fn(xraw_join[[child_raw]], ...)
  xref_join[[child_ref]] <- std_fn(xref_join[[child_ref]], ...)

  xraw_split <- split(xraw_join, xraw_join$TEMP_ROW_ID)
  xref_split <- split(xref_join, xref_join$TEMP_ROW_ID)

  group_vars <- c("TEMP_ROW_ID", prep$by_ref[1:level])

  match_l <- mapply(match_parents,
                    xraw_i = xraw_split,
                    xref_i = xref_split,
                    child_raw = child_raw,
                    child_ref = child_ref,
                    MoreArgs = list(group_vars = group_vars,
                                    max_dist = max_dist,
                                    mmin = mmin,
                                    pmin = pmin),
                    SIMPLIFY = FALSE)

  match_join <- dplyr::bind_rows(match_l)

  match_join_ref <- left_join(match_join, ref_prep_split[[level]], by = prep$by_ref[1:level])
  match_join_ref <- match_join_ref[,c("TEMP_ROW_ID", names(prep$ref), "m", "nraw", "nref")]

  xjoin <- x[,setdiff(names(x), names(prep$ref))]
  out <- dplyr::left_join(xjoin, match_join_ref, by = "TEMP_ROW_ID")

  out <- out[,!names(out) %in% "TEMP_ROW_ID"]

  return(out)
}



#' @noRd
#' @importFrom dplyr `%>%` mutate group_by_at ungroup summarize filter n
#' @importFrom stringdist stringdistmatrix
match_parents <- function(xraw_i,
                          xref_i,
                          child_raw,
                          child_ref,
                          group_vars,
                          max_dist,
                          mmin,
                          pmin) {

  dmat <- stringdist::stringdistmatrix(
    xraw_i[[child_raw]],
    xref_i[[child_ref]]
  )

  xref_i %>%
    mutate(match = apply(dmat <= max_dist, 2, any)) %>%
    group_by_at(group_vars) %>%
    summarize(m = sum(match),
              nraw = length(xraw_i[[child_raw]]),
              nref = n()) %>%
    ungroup() %>%
    filter(m >= mmin, m / nraw >= pmin)
}


#' @noRd
#' @importFrom dplyr bind_rows
prep_xref <- function(x, y, by) {
  xs <- split(x, x$TEMP_ROW_ID)
  dplyr::bind_rows(lapply(xs, prep_xref_join, y = y, by = by))
}


#' @noRd
#' @importFrom dplyr bind_cols left_join
prep_xref_join <- function(x, y, by) {
  if (all(is.na(x[,by]))) {
    out <- dplyr::bind_cols(TEMP_ROW_ID = rep(unique(x$TEMP_ROW_ID), nrow(y)), y)
  } else {
    out <- dplyr::left_join(x, y, by = by)
  }
  return(out)
}


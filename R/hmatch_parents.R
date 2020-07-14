#' Hierarchical matching of parents based on sets of common offspring
#'
#' @description
#' Match a hierarchical column (e.g. region, province, or county) within a raw,
#' potentially messy dataset against a corresponding column within a reference
#' dataset, by searching for similar sets of 'offspring' (i.e. values at the
#' next hierarchical level).
#'
#' For example, if the raw dataset uses admin1 level "NY" whereas the reference
#' dataset uses "New York", it would be difficult to automatically match these
#' values using only fuzzy-matching. However, we might nonetheless be able to
#' match "NY" to "New York" if they share a common and unique set of 'offspring'
#' (i.e. admin2 values) across both datasets (e.g "Kings", "Queens", "New York",
#' "Suffolk", "Bronx", etc.).
#'
#' @inheritParams hmatch
#'
#' @param x a `data.frame` representing one or more rows from `raw`, and
#'   optionally including matching columns from `ref` to help narrow down the
#'   set of possible offspring to try matching
#' @param level integer index of the hierarchical level to match at
#' @param mmin minimum number of matching offspring required for parents to be
#'   considered a match
#' @param pmin minimum proportion of matching offspring required for parents to
#'   be considered a match (i.e. the proportion of offspring in `raw` with match
#'   in `ref`)
#' @param type type of join ("inner" or "left") (defaults to "left")
#'
#' @return a data frame obtained by matching the hierarchical columns in `raw`
#'   and `ref`, using the join type specified by argument `type` (see
#'   \link{join_types} for more details)
#'
#' @examples
#' # prepare example data
#' raw <- ne_ref
#' raw$adm1[raw$adm1 == "Ontario"] <- "ON"
#' raw$adm1[raw$adm1 == "New York"] <- "NY"
#' raw$adm1[raw$adm1 == "New Jersey"] <- "NJ"
#' raw$adm1[raw$adm1 == "Pennsylvania"] <- "PA"
#' x <- raw[raw$level == 1,]
#'
#' hmatch_parents(
#'   x,
#'   raw,
#'   ne_ref,
#'   pattern = "adm",
#'   level = 2,
#'   mmin = 2,
#'   pmin = 0.5,
#'   type = "inner"
#' )
#'
#' @importFrom dplyr left_join bind_rows
#' @export hmatch_parents
hmatch_parents <- function(x,
                           raw,
                           ref,
                           level,
                           mmin,
                           pmin,
                           pattern,
                           pattern_ref = pattern,
                           by,
                           by_ref = by,
                           type = "left",
                           ref_prefix = "ref_",
                           fuzzy = FALSE,
                           fuzzy_method = "osa",
                           fuzzy_dist = 1L,
                           std_fn = string_std,
                           ...) {

  # # for testing purposes only
  # raw <- ne_ref
  # raw$adm1[raw$adm1 == "New York"] <- "NY"
  # x <- raw[raw$level == 1,]
  # ref <- ne_ref
  # pattern = "adm"
  # pattern_ref = pattern
  # level <- 2
  # by = NULL
  # dict <- NULL
  # type = "left"
  # ref_prefix = "ref_"
  # std_fn = string_std
  # fuzzy_dist <- 1
  # mmin = 2
  # pmin = 0.5
  # ... <- NULL

  ## match args
  type <- match.arg(type, c("left", "inner"))

  ## temp cols
  temp_col_id <- "TEMP_ROW_ID"
  temp_col_match <- "TEMP_MATCH"

  x[[temp_col_id]] <- seq_len(nrow(x))

  ## identify hierarchical columns to match, and rename ref cols if necessary
  prep <- prep_match_columns(
    raw = raw,
    ref = ref,
    pattern = pattern,
    pattern_ref = pattern_ref,
    by = by,
    by_ref = by_ref,
    ref_prefix = ref_prefix
  )

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
                                    fuzzy_method = fuzzy_method,
                                    fuzzy_dist = fuzzy_dist,
                                    mmin = mmin,
                                    pmin = pmin),
                    SIMPLIFY = FALSE)

  match_join <- dplyr::bind_rows(match_l)
  match_join[[temp_col_match]] <- TRUE

  match_join_ref <- left_join(match_join, ref_prep_split[[level]], by = prep$by_ref[1:level])
  match_join_ref <- match_join_ref[,c(temp_col_id, temp_col_match, names(prep$ref), "m", "nraw", "nref")]

  xjoin <- x[,setdiff(names(x), names(prep$ref))]
  matches_out <- dplyr::left_join(xjoin, match_join_ref, by = temp_col_id)

  ## execute match type and remove temporary columns
  prep_output(
    x = matches_out,
    type = type,
    temp_col_id = temp_col_id,
    temp_col_match = temp_col_match,
    cols_raw_orig = names(raw),
    class_raw = class(x)
  )
}




#' @noRd
#' @importFrom dplyr left_join
#' @importFrom stats aggregate
#' @importFrom stringdist stringdistmatrix
match_parents <- function(xraw_i,
                          xref_i,
                          child_raw,
                          child_ref,
                          group_vars,
                          fuzzy_method,
                          fuzzy_dist,
                          mmin,
                          pmin) {

  dmat <- stringdist::stringdistmatrix(
    xraw_i[[child_raw]],
    xref_i[[child_ref]],
    method = fuzzy_method
  )

  xref_i$match <- apply(dmat <= fuzzy_dist, 2, any)

  out_orig <- stats::aggregate(
    list(m = xref_i$match),
    by = as.list(xref_i[group_vars]),
    sum
  )

  out_orig$nraw <- length(xraw_i[[child_raw]])

  out_nref <- stats::aggregate(
    list(nref = xref_i$match),
    by = as.list(xref_i[group_vars]),
    length
  )

  out <- dplyr::left_join(out_orig, out_nref, by = group_vars)

  out[out$m >= mmin & out$m / out$nraw >= pmin,]
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


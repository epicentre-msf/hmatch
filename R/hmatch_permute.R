#' Hierarchical matching with sequential column permutation to allow for values
#' entered at the wrong hierarchical level
#'
#' @description
#' Match a data frame with raw, potentially messy hierarchical data (e.g.
#' province, county, township) against a reference dataset, using sequential
#' permutation of the hierarchical columns to allow for values entered at the
#' wrong hierarchical level.
#'
#' The function calls \code{\link{hmatch_partial}} on each possible permutation
#' of the hierarchical columns, and then combines the results. Rows of `raw`
#' yielding multiple matches to `ref` can optionally be resolved using a
#' resolve-type join (see section **Resolve joins** below).
#'
#' @inheritParams hmatch_partial
#'
#' @return a data frame obtained by matching the hierarchical columns in `raw`
#'   and `ref`, using the join type specified by argument `type` (see
#'   \link{join_types} for more details)
#'
#' @section Resolve joins:
#' In `hmatch_permute`, if argument `type` corresponds to a resolve join, rows
#' of `raw` with multiple matches to `ref` are resolved to the highest
#' hierarchical level that is common among all matches (or no match if there is
#' a conflict at the very first level). E.g.
#'
#' `raw`: \cr
#' `1. | United States | <NA>     | New York |` \cr
#'
#' Relevant rows from `ref`: \cr
#' `1. | United States | New York | <NA>     |` \cr
#' `2. | United States | New York | New York |` \cr
#'
#' In a regular join with `hmatch_permute`, the single row from `raw` (above)
#' will match both of the depicted rows from `ref`. However, in a resolve join
#' the two matches will resolve to the first row from `ref`, because it reflects
#' the highest hierarchical level that is common to all matches.
#'
#' @examples
#' data(ne_raw)
#' data(ne_ref)
#'
#' hmatch_permute(ne_raw, ne_ref, pattern = "^adm", type = "inner")
#'
#' @importFrom dplyr left_join bind_rows
#' @export hmatch_permute
hmatch_permute <- function(raw,
                           ref,
                           pattern = NULL,
                           pattern_ref = pattern,
                           by = NULL,
                           by_ref = by,
                           type = "left",
                           dict = NULL,
                           ref_prefix = "ref_",
                           fuzzy = FALSE,
                           max_dist = 1L,
                           std_fn = string_std,
                           ...) {


  # # raw <- drc_raw_low
  # # ref <- drc_ref
  # raw <- ne_raw
  # ref <- ne_ref
  # pattern = "adm"
  # pattern_ref = pattern
  # by = NULL
  # by_ref = by
  # dict = NULL
  # type = "left"
  # ref_prefix = "ref_"
  # fuzzy = FALSE
  # max_dist = 1L
  # std_fn = string_std
  # ... <- NULL


  ## match args
  if (!is.null(std_fn)) std_fn <- match.fun(std_fn)
  type <- match.arg(type, c("left", "inner", "anti", "resolve_left", "resolve_inner", "resolve_anti"))

  ## temporary columns to aid in matching
  temp_col_code <- "TEMP_CODE_COL_PERMUTE"
  temp_col_id <- "TEMP_ROW_ID_PERMUTE"

  raw[[temp_col_id]] <- seq_len(nrow(raw))

  ## identify hierarchical columns to match, and rename ref cols if necessary
  prep <- prep_match_columns(
    raw = raw,
    ref = ref,
    pattern = pattern,
    pattern_ref = pattern_ref,
    by = by,
    by_ref = by_ref,
    ref_prefix = ref_prefix,
    code_col = temp_col_code
  )

  ## add standardized columns for joining
  raw_join <- add_join_columns(
    dat = raw,
    by = prep$by_raw,
    join_cols = prep$by_raw_join,
    std_fn = std_fn,
    ...
  )

  ref_join <- add_join_columns(
    dat = prep$ref,
    by = prep$by_ref,
    join_cols = prep$by_ref_join,
    std_fn = std_fn,
    ...
  )

  ## implement dictionary recoding on join columns
  # TODO: incorporate into permute_columns to get recoding at *permuted* level
  if (!is.null(dict)) {
    raw_join <- apply_dict(
      raw_join,
      dict,
      by_raw = prep$by_raw,
      by_join = prep$by_raw_join,
      std_fn = std_fn
    )
  }

  ## generate every possible permutation of join columns
  raw_join_permutations <- permute_columns(raw_join, prep$by_raw_join)

  ## call hmatch_partial_ for every permutation
  raw_perm_match <- mapply(
    hmatch_partial_,
    raw_join = raw_join_permutations,
    MoreArgs = list(
      ref_join = ref_join,
      by_raw_join = prep$by_raw_join,
      by_ref_join = prep$by_ref_join,
      fuzzy = fuzzy,
      max_dist = max_dist,
      type = "inner"
    ),
    SIMPLIFY = FALSE
  )

  ## bind matches
  matches_join_prep <- unique(dplyr::bind_rows(raw_perm_match))
  matches_join_prep <- matches_join_prep[,c(temp_col_id, names(prep$ref))]

  ## if resolve join
  if (grepl("^resolve", type)) {
    matches_join_prep <- resolve_join(
      matches_join_prep,
      by_ref = prep$by_ref,
      temp_col_id = temp_col_id,
      consistent = "min"
    )
  }

  ## assemble output
  matches_out <- dplyr::left_join(raw, matches_join_prep, by = temp_col_id)

  ## execute match type and remove temporary columns
  prep_output(
    x = matches_out,
    type = gsub("^resolve_", "", type),
    temp_col_id = temp_col_id,
    temp_col_match = temp_col_code,
    cols_raw_orig = setdiff(names(raw), temp_col_id),
    class_raw = class(raw)
  )
}


#' @noRd
permute_columns <- function(x, by_x) {
  # TODO: could make this more efficient by permuting based on n_levels
  len_x <- length(by_x)
  xx <- replicate(len_x, by_x, simplify = FALSE)
  xx <- expand.grid(xx)
  rows <- apply(xx, 1, function(x) length(unique(x)) == len_x)
  xx <- xx[rows,, drop = FALSE]
  col_permutations <- as.list(as.data.frame(t(xx)))
  lapply(col_permutations, rename_helper, x = x, name = by_x)
}


#' @noRd
#' @importFrom dplyr rename all_of
rename_helper <- function(x, name, name_replace) {
  vec_rename <- name
  names(vec_rename) <- name_replace
  x <- dplyr::rename(x, all_of(vec_rename))
  names_other <- setdiff(names(x), name)
  x[,c(name, names_other), drop = FALSE]
}


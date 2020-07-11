#' Hierarchical matching with sequential column permutation to allow for values
#' entered at wrong hierarchical level
#'
#' @description
#' Match a data.frame with raw, potentially messy hierarchical data (e.g.
#' province, county, township) against a reference dataset, using sequential
#' permutation of the hierarchical columns to allow for values entered at the
#' wrong hierarchical level.
#'
#' The function calls \code{\link{hmatch_partial}} on each possible permutation
#' of the hierarchical columns, and then resolves the returned match data to a
#' maximum of one unique match per row of `raw`.
#'
#' If the various permutations of a given row of `raw` match multiple rows
#' within `ref` that are hierarchically consistent, then the broadest-level
#' match will be returned. E.g. a raw value "New York" could match at both the
#' state and county-level. The matches are consistent because New York county is
#' a child of New York state, and so the broader-level match (the state level)
#' will be returned.
#'
#' Alternatively, if the permutations of a given row of `raw` match multiple
#' conflicting rows within `ref`, then no match will be returned for that row.
#' E.g. a raw value "Suffolk" could match multiple counties in different states,
#' Suffolk NY, Suffolk MA, etc, which are not hierarchically consistent.
#'
#' @inheritParams hmatch_partial
#'
#' @param type type of join ("left", "inner", "anti"). Defaults to "left". See
#'   \link{join_types}.
#'
#' @return a data frame obtained by matching the hierarchical columns in `raw`
#'   and `ref`, using the join type specified by argument `type` (see
#'   \link{join_types} for more details)
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
  type <- match.arg(type, c("left", "inner", "inner_unique", "anti", "anti_unique"))
  # TODO: think more about match types. left should return all matches even if inconsistent

  ## temporary columns to aid in matching
  temp_col_code <- "TEMP_CODE_COL_SHIFT"
  temp_col_id <- "TEMP_ROW_ID_SHIFT"

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
  matches_bind <- dplyr::bind_rows(raw_perm_match)
  matches_bind <- unique(matches_bind[,c(temp_col_id, temp_col_code)])

  ## resolve best code for each id
  codes_by_id <- split(
    matches_bind[[temp_col_code]],
    matches_bind[[temp_col_id]]
  )

  codes_by_id_resolved <- vapply(
    codes_by_id,
    resolve_geocode,
    "",
    ref = prep$ref,
    by_ref = prep$by_ref,
    temp_col_code = temp_col_code
  )

  matches_join_init <- data.frame(
    id = as.integer(names(codes_by_id_resolved)),
    codes_by_id_resolved,
    stringsAsFactors = FALSE
  )

  names(matches_join_init) <- c(temp_col_id, temp_col_code)

  ## assemble output
  matches_join <- dplyr::left_join(matches_join_init, prep$ref, by = temp_col_code)
  matches_out <- dplyr::left_join(raw, matches_join, by = temp_col_id)

  ## execute match type and remove temporary columns
  prep_output(
    x = matches_out,
    type = type,
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

  lapply(
    col_permutations,
    rename_helper,
    x = x,
    name = by_x
  )
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


#' @noRd
resolve_geocode <- function(x, ref, by_ref, temp_col_code) {

  ref_sub <- unique(ref[ref[[temp_col_code]] %in% x,, drop = FALSE])

  if (nrow(ref_sub) == 0L) {
    out <- NA_character_
  } else if (nrow(ref_sub) == 1L) {
    out <- ref_sub[[temp_col_code]][1L]
  } else {
    ref_sub_ <- ref_sub[,by_ref, drop = FALSE]
    matches_consistent <- all(vapply(ref_sub_, unique_excl_na, FALSE))
    if (!matches_consistent) {
      out <- NA_character_
    } else {
      max_ref_levels <- max_levels(ref_sub_)
      is_min_ref_level <- max_ref_levels == min(max_ref_levels)
      if (sum(is_min_ref_level) > 1L) {
        out <- NA_character_
      } else {
        out <- ref_sub[[temp_col_code]][is_min_ref_level]
      }
    }
  }
  return(out)
}


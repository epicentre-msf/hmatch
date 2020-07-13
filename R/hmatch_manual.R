#' Manual hierarchical matching
#'
#' Match a data.frame with raw, potentially messy hierarchical data (e.g.
#' province, county, township) against a reference dataset, using a dictionary
#' of manually-specified matches.
#'
#' @inheritParams hmatch
#'
#' @param man `data.frame` of manually-specified matches, relating a given set
#'   of hierarchical values to the code within `ref` to which those values
#'   correspond
#' @param pattern regex pattern to match the hierarchical columns in `raw`
#'   and `man` (see also \link{specifying_columns})
#' @param by vector giving the names of the hierarchical columns in `raw` and
#'   `man`
#' @param code_col name of the code column containing codes for matching `ref`
#'   and `man`
#' @param type type of join ("left", "inner", or "anti"). Defaults to "left".
#'   See \link{join_types}. Note that this function does not allow 'resolve
#'   joins', unlike most other `hmatch_` functions.
#'
#' @return a data frame obtained by matching the hierarchical columns in `raw`
#'   and `ref` based on sets of matches specified in `man`, using the join type
#'   specified by argument `type` (see \link{join_types} for more details)
#'
#' @examples
#' data(ne_raw)
#' data(ne_ref)
#'
#' # create df mapping sets of raw hierarchical values to codes within ref
#' ne_man <- data.frame(adm0 = NA_character_,
#'                      adm1 = NA_character_,
#'                      adm2 = "NJ_Bergen",
#'                      hcode = "211",
#'                      stringsAsFactors = FALSE)
#'
#' # find manual matches
#' hmatch_manual(ne_raw, ne_ref, ne_man, code_col = "hcode", type = "inner")
#'
#' @importFrom dplyr inner_join
#' @export hmatch_manual
hmatch_manual <- function(raw,
                          ref,
                          man,
                          pattern = NULL,
                          pattern_ref = pattern,
                          by = NULL,
                          by_ref = by,
                          code_col,
                          type = "left",
                          ref_prefix = "ref_",
                          std_fn = string_std,
                          ...) {

  # # for testing purposes only
  # raw <- ne_raw
  # ref <- ne_ref
  # man <- data.frame(adm0 = NA_character_,
  #                   adm1 = NA_character_,
  #                   adm2 = "NJ_Bergen",
  #                   hcode = "211",
  #                   stringsAsFactors = FALSE)
  # pattern = NULL
  # pattern_ref = pattern
  # by = NULL
  # by_ref = by
  # code_col <- "hcode"
  # type = "left"
  # ref_prefix = "ref_"
  # std_fn = string_std
  # ... <- NULL

  ## match args
  if (!is.null(std_fn)) std_fn <- match.fun(std_fn)
  type <- match.arg(type, c("left", "inner", "anti"))

  ## validate arg code_col
  if (code_col %in% names(raw)) {
    warning("`code_col` already exists in `raw`, and will be overwritten")
    raw <- raw[!names(raw) %in% code_col]
  }

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

  ## join ref to man by code_col
  man_ref <- dplyr::inner_join(
    prep$ref,
    man,
    by = code_col
  )

  ## add standardized columns for joining
  raw_join <- add_join_columns(
    dat = raw,
    by = prep$by_raw,
    join_cols = prep$by_raw_join,
    std_fn = std_fn,
    ...
  )

  man_join <- add_join_columns(
    dat = man_ref,
    by = prep$by_raw,
    join_cols = prep$by_raw_join,
    std_fn = std_fn,
    ...
  )

  ## run main matching routines
  hmatch_manual_(
    raw_join = raw_join,
    man_join = man_join,
    by_raw = prep$by_raw,
    by_ref = prep$by_ref,
    by_join = prep$by_raw_join,
    type = type,
    class_raw = class(raw)
  )
}


#' @noRd
#' @importFrom dplyr left_join
hmatch_manual_ <- function(raw_join,
                           man_join,
                           by_raw,
                           by_ref,
                           by_join,
                           type = "left",
                           class_raw = "data.frame") {


  ## add temporary row-id column to aid in matching
  temp_col_id <- "TEMP_ROW_ID_MANUAL"
  raw_join[[temp_col_id]] <- seq_len(nrow(raw_join))

  ## add temporary match column to ref_join
  temp_col_match <- "TEMP_MATCH_MANUAL"
  man_join[[temp_col_match]] <- TRUE

  ## re-derive initial (pre-join) column names
  names_raw_prep <- setdiff(names(raw_join), by_join)
  names_raw_orig <- setdiff(names_raw_prep, temp_col_id)

  ## remove extraneous columns and filter to unique rows
  man_join_final <- unique(man_join[!names(man_join) %in% by_raw])

  ## merge raw and man
  matches_out <- dplyr::left_join(
    raw_join,
    man_join_final,
    by = by_join
  )

  ## remove join cols
  matches_out <- matches_out[, !names(matches_out) %in% by_join, drop = FALSE]

  ## check for rows of raw matched by multiple different entries in man
  matches_out_check <- matches_out[!is.na(matches_out[[temp_col_match]]),]

  if (nrow(matches_out_check) > 0) {
    n_codes_per_id <- stats::aggregate(
      list(n_codes = matches_out_check[[temp_col_match]]),
      list(temp_id = matches_out_check[[temp_col_id]]),
      function(x) length(unique(x))
    )

    if (any(n_codes_per_id$n_codes > 1L)) {
      warning("One or more rows or `raw` matched by multiple entries in `man`",
              call. = FALSE)
    }
  }

  # ## if resolve-type join
  # if (grepl("^resolve", type)) {
  #   matches_out <- resolve_join(
  #     matches_out,
  #     by_ref = by_ref,
  #     temp_col_id = temp_col_id,
  #     consistent = "all"
  #   )
  # }

  ## execute match type and remove temporary columns
  prep_output(
    x = matches_out,
    type = type,
    temp_col_id = temp_col_id,
    temp_col_match = temp_col_match,
    cols_raw_orig = names_raw_orig,
    class_raw = class_raw
  )
}


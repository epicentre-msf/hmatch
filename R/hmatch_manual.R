#' Manual hierarchical matching
#'
#' Match a data.frame with raw, potentially messy hierarchical data (e.g.
#' province, county, township) against a reference dataset, using a dictionary
#' of manually-specified matches.
#'
#' @inheritParams hmatch_complete
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
#' hmatch_manual(ne_raw, ne_ref, ne_man, code_col = "hcode")
#'
#' @importFrom dplyr left_join
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
                          concise = FALSE,
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
  # concise = FALSE
  # ref_prefix = "ref_"
  # std_fn = string_std
  # ... <- NULL

  if (!is.null(std_fn)) std_fn <- match.fun(std_fn)
  type <- match.arg(type, c("left", "inner", "inner_unique", "anti", "anti_unique"))

  if (code_col %in% names(raw)) {
    warning("`code_col` already exists in `raw`, and will be overwritten")
    raw <- raw[!names(raw) %in% code_col]
  }

  ## create temporary row id in raw
  names_raw_orig <- names(raw)
  temp_id_col <- "TEMP_ROW_ID_MAN"
  raw[[temp_id_col]] <- seq_len(nrow(raw))

  ## identify hierarchical columns to match, and rename ref cols if necessary
  prep <- prep_match_columns(raw = raw,
                             ref = ref,
                             pattern = pattern,
                             pattern_ref = pattern_ref,
                             by = by,
                             by_ref = by_ref,
                             ref_prefix = ref_prefix)

  ## join ref to man by code_col
  man_ref <- merge(man, prep$ref, all.x = TRUE)

  ## add standardized columns for joining
  raw_join <- add_join_columns(dat = raw,
                               by = prep$by_raw,
                               join_cols = prep$by_join,
                               std_fn = std_fn,
                               ...)

  man_join <- add_join_columns(dat = man_ref,
                               by = prep$by_raw,
                               join_cols = prep$by_join,
                               std_fn = std_fn,
                               ...)

  ## check for duplicated rows in man after standardization
  if (any(duplicated(man_join[, prep$by_join, drop = FALSE]))) {
    warning("Duplicated rows in `man` after standardization")
  }

  ## remove extraneous columns from raw, and filter to unique rows
  man_join_final <- unique(man_join[!names(man_join) %in% prep$by_raw])

  ## merge raw and man
  out <- dplyr::left_join(raw_join, man_join_final, by = prep$by_join)

  ## execute merge type
  dup_ids <- out[[temp_id_col]][duplicated(out[[temp_id_col]])]

  if (type == "inner") {
    out <- out[!is.na(out[[code_col]]),]
  } else if (type == "inner_unique") {
    rows_keep <- !is.na(out[[code_col]]) & !out[[temp_id_col]] %in% dup_ids
    out <- out[rows_keep,]
  } else if (type == "anti") {
    out <- out[is.na(out[[code_col]]), names(raw)]
  } else if (type == "anti_unique") {
    rows_keep <- is.na(out[[code_col]]) | out[[temp_id_col]] %in% dup_ids
    out <- unique(out[rows_keep, names(raw)])
  }

  ## reclass out to match raw (tibble classes with otherwise be stripped)
  class(out) <- class(raw)

  ## remove temporary columns and return
  out <- out[,!names(out) %in% c(prep$by_join, temp_id_col), drop = FALSE]
  if (concise) out <- out[,c(prep$by_raw, prep$by_ref)]

  return(out)
}


#' Hierarchical matching with tokenization of multi-term values
#'
#' @description
#' Match sets of hierarchical values (e.g. province / county / township) in a
#' raw, messy dataset to corresponding values within a reference dataset, using
#' tokenization to help match multi-term values that might otherwise be
#' difficult to match (e.g. "New York City" vs. "New York").
#'
#' Includes options for ignoring matches from frequently-occurring tokens (e.g.
#' "North", "South", "City"), small tokens (e.g. "El", "San", "New"), or any
#' other set of tokens specified by the user.
#'
#' @inheritParams hmatch
#' @inherit hmatch return
#'
#' @param always_tokenize logical indicating whether to tokenize all values
#'   prior to matching (`TRUE`), or to first attempt non-tokenized matching with
#'   \code{\link{hmatch}} and only tokenize values within `raw` (and
#'   corresponding putative matches within `ref`) that don't have a
#'   non-tokenized match (`FALSE`). Defaults to `FALSE`.
#' @param token_split regex pattern to split strings into tokens. Currently
#'   tokenization is implemented *after*
#'   \link[=string_standardization]{string-standardizatipn} with argument
#'   `std_fn` (this may change in a future version), so the regex pattern should
#'   split *standardized* strings rather than the original strings. Defaults to
#'   "_".
#' @param exclude_freq exclude tokens from matching if they have a frequency
#'   greater than or equal to this value. Refers to the number of unique,
#'   string-standardized values at a given hierarchical level in which a given
#'   token occurs, as calculated by \code{\link{count_tokens}} (separately for
#'   `raw` and `ref`). Defaults to `3`.
#' @param exclude_nchar exclude tokens from matching if they have \link{nchar}
#'   less than or equal to this value. Defaults to `3`.
#' @param exclude_values character vector of additional tokens to exclude from
#'   matching. Subject to \link[=string_standardization]{string-standardizatipn}
#'   with argument `std_fn`.
#'
#' @return a data frame obtained by matching the hierarchical columns in `raw`
#'   and `ref`, using the join type specified by argument `type` (see
#'   \link{join_types} for more details)
#'
#' @section Resolve joins:
#' Uses the same approach to resolve joins as \code{\link{hmatch}}.
#'
#' @examples
#' data(ne_raw)
#' data(ne_ref)
#'
#' # add tokens to some values within ref to illustrate tokenized matching
#' ne_ref$adm0[ne_ref$adm0 == "United States"] <- "United States of America"
#' ne_ref$adm1[ne_ref$adm1 == "New York"] <- "New York State"
#'
#' hmatch_tokens(ne_raw, ne_ref, type = "inner")
#'
#' @export hmatch_tokens
hmatch_tokens <- function(raw,
                          ref,
                          pattern,
                          pattern_ref = pattern,
                          by,
                          by_ref = by,
                          type = "left",
                          allow_gaps = TRUE,
                          always_tokenize = FALSE,
                          token_split = "_",
                          exclude_freq = 3,
                          exclude_nchar = 3,
                          exclude_values = NULL,
                          fuzzy = FALSE,
                          fuzzy_method = "osa",
                          fuzzy_dist = 1L,
                          dict = NULL,
                          ref_prefix = "ref_",
                          std_fn = string_std,
                          ...) {

  # raw <- readRDS("~/desktop/drc_bench_raw.rds")[1:5000,1:4]
  # ref <- readRDS("~/desktop/drc_bench_ref.rds")
  # pattern = NULL
  # pattern_ref = pattern
  # by = NULL
  # by_ref = by
  # type = "inner"
  # allow_gaps = TRUE
  # always_tokenize = FALSE
  # token_split = "_"
  # exclude_freq = 5
  # exclude_nchar = 3
  # exclude_values = c("Nord", "Sud")
  # ref_prefix = "ref_"
  # fuzzy = TRUE
  # fuzzy_method = "osa"
  # fuzzy_dist = 1L
  # dict <- NULL
  # std_fn = string_std
  # ... <- NULL


  ## match args
  if (!is.null(std_fn)) std_fn <- match.fun(std_fn)
  type <- match.arg(type, c("left", "inner", "anti", "resolve_left", "resolve_inner", "resolve_anti"))

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

  ## standardize exclude values
  if (!is.null(std_fn)) exclude_values <- std_fn(exclude_values, ...)

  ## implement dictionary recoding on join columns
  if (!is.null(dict)) {
    raw_join <- apply_dict(
      raw_join,
      dict,
      by_raw = prep$by_raw,
      by_join = prep$by_raw_join,
      std_fn = std_fn
    )
  }

  ## tokens to exclude based on frequency
  exclude_freq_raw_l <- lapply(
    raw_join[,prep$by_raw_join, drop = FALSE],
    exclude_tokens_freq,
    min_freq = exclude_freq
  )

  exclude_freq_ref_l <- lapply(
    ref_join[,prep$by_ref_join, drop = FALSE],
    exclude_tokens_freq,
    min_freq = exclude_freq
  )

  ## run main matching routine
  hmatch_tokens_(
    raw_join = raw_join,
    ref_join = ref_join,
    by_raw = prep$by_raw,
    by_ref = prep$by_ref,
    by_raw_join = prep$by_raw_join,
    by_ref_join = prep$by_ref_join,
    allow_gaps = allow_gaps,
    type = type,
    always_tokenize = always_tokenize,
    token_split = token_split,
    exclude_freq_raw_l = exclude_freq_raw_l,
    exclude_freq_ref_l = exclude_freq_ref_l,
    exclude_nchar = exclude_nchar,
    exclude_values = exclude_values,
    fuzzy = fuzzy,
    fuzzy_method = fuzzy_method,
    fuzzy_dist = fuzzy_dist,
    class_raw = class(raw)
  )
}


#' @noRd
#' @importFrom dplyr inner_join left_join
hmatch_tokens_ <- function(raw_join,
                           ref_join,
                           by_raw = NULL, # not used
                           by_ref = NULL, # only used if type is resolve join
                           by_raw_join,
                           by_ref_join,
                           allow_gaps,
                           type,
                           always_tokenize,
                           token_split,
                           exclude_freq_raw_l,
                           exclude_freq_ref_l,
                           exclude_nchar,
                           exclude_values,
                           fuzzy,
                           fuzzy_method,
                           fuzzy_dist,
                           class_raw) {


  ## add temporary row-id column to aid in matching
  temp_col_id <- "TEMP_ROW_ID_PART"
  raw_join[[temp_col_id]] <- seq_len(nrow(raw_join))

  ## add temporary match column to ref_join
  temp_col_match <- "TEMP_MATCH_PART"
  ref_join[[temp_col_match]] <- rep(TRUE, nrow(ref_join))

  ## re-derive initial (pre-join) column names
  names_raw_prep <- setdiff(names(raw_join), by_raw_join)
  names_raw_orig <- setdiff(names_raw_prep, temp_col_id)
  names_ref_prep <- setdiff(names(ref_join), by_ref_join)

  ## add max non-missing adm level
  temp_col_max_raw <- "MAX_ADM_RAW_"
  temp_col_max_ref <- "MAX_ADM_REF_"
  raw_join[[temp_col_max_raw]] <- max_levels(raw_join, by = by_raw_join)
  ref_join[[temp_col_max_ref]] <- max_levels(ref_join, by = by_ref_join)

  ## if !allow_gaps, filter now to complete sequences for efficiency
  raw_join_orig <- raw_join

  if (!allow_gaps) {
    rows_no_gaps <- complete_sequence(raw_join, by_raw_join)
    raw_join <- raw_join[rows_no_gaps, , drop = FALSE]
  }

  ## extract only the join columns
  raw_ <- raw_join[,by_raw_join, drop = FALSE]
  ref_ <- ref_join[,by_ref_join, drop = FALSE]

  ## identify the min and maximum hierarchical levels
  max_level <- length(by_raw_join)

  col_max_raw <- by_raw_join[max_level]
  col_max_ref <- by_ref_join[max_level]

  col_min_raw <- by_raw_join[1]
  col_min_ref <- by_ref_join[1]

  ## raw/ref combinations at first hierarchical level
  initial_combinations <- expand.grid(
    x = unique(raw_[[col_min_raw]]),
    y = unique(ref_[[col_min_ref]]),
    stringsAsFactors = FALSE
  )

  names(initial_combinations) <- c(col_min_raw, col_min_ref)

  ## filter to actual matches at first hierarchical level
  matches_remaining <- filter_to_matches_tokenize(
    x = initial_combinations,
    col_raw = col_min_raw,
    col_ref = col_min_ref,
    always_tokenize = always_tokenize,
    token_split = token_split,
    exclude_freq_raw = exclude_freq_raw_l[[1]],
    exclude_freq_ref = exclude_freq_ref_l[[1]],
    exclude_nchar = exclude_nchar,
    exclude_values = exclude_values,
    fuzzy = fuzzy,
    fuzzy_method = fuzzy_method,
    fuzzy_dist = fuzzy_dist,
    is_max_level = max_level == 1L
  )

  ## for each subsequent hierarchical level...
  if (max_level > 1) {
    for (j in 2:max_level) {

      ## identify relevant columns
      col_focal_raw <- by_raw_join[j]
      col_focal_ref <- by_ref_join[j]

      cols_prev_raw <- by_raw_join[1:(j - 1)]
      cols_prev_ref <- by_ref_join[1:(j - 1)]

      col_up_to_focal_raw <- by_raw_join[1:j]
      col_up_to_focal_ref <- by_ref_join[1:j]

      ## prepare dfs for joining next hierarchical level in raw and ref
      next_join_raw <- unique(raw_[,col_up_to_focal_raw, drop = FALSE])
      next_join_ref <- unique(ref_[,col_up_to_focal_ref, drop = FALSE])

      ## join next levels of raw and ref
      matches_remaining <- dplyr::inner_join(
        matches_remaining,
        next_join_raw,
        by = cols_prev_raw
      )

      matches_remaining <- dplyr::inner_join(
        matches_remaining,
        next_join_ref,
        by = cols_prev_ref
      )

      ## filter to matches at current hierarchical level
      matches_remaining <- filter_to_matches_tokenize(
        x = matches_remaining,
        col_raw = col_focal_raw,
        col_ref = col_focal_ref,
        always_tokenize = always_tokenize,
        token_split = token_split,
        exclude_freq_raw = exclude_freq_raw_l[[j]],
        exclude_freq_ref = exclude_freq_ref_l[[j]],
        exclude_nchar = exclude_nchar,
        exclude_values = exclude_values,
        fuzzy = fuzzy,
        fuzzy_method = fuzzy_method,
        fuzzy_dist = fuzzy_dist,
        is_max_level = col_focal_raw == col_max_raw
      )
    }
  }

  ## match bare join columns back to raw_join and ref_join
  matches_join_out <- dplyr::inner_join(
    raw_join[, c(temp_col_id, temp_col_max_raw, by_raw_join)],
    matches_remaining,
    by = by_raw_join
  )

  matches_join_out <- dplyr::inner_join(
    matches_join_out,
    ref_join,
    by = by_ref_join
  )

  ## filter to matches where the max ref level is <= the max raw level
  keep <- matches_join_out[[temp_col_max_ref]] <= matches_join_out[[temp_col_max_raw]]
  matches_join_out <- matches_join_out[keep, , drop = FALSE]

  ## remove join columns and filter to unique rows
  matches_join_out <- unique(matches_join_out[,c(temp_col_id, names_ref_prep)])

  ## if resolve-type join
  if (grepl("^resolve", type)) {
    matches_join_out <- resolve_join(
      matches_join_out,
      by_ref = by_ref,
      temp_col_id = temp_col_id,
      consistent = "all"
    )
  }

  ## merge raw with final match data
  raw_join_out <- raw_join_orig[,names_raw_prep, drop = FALSE]
  matches_out <- dplyr::left_join(raw_join_out, matches_join_out, by = temp_col_id)

  ## execute match type and remove temporary columns
  prep_output(
    x = matches_out,
    type = gsub("^resolve_", "", type),
    temp_col_id = temp_col_id,
    temp_col_match = temp_col_match,
    cols_raw_orig = names_raw_orig,
    class_raw = class_raw
  )
}



#' @noRd
#' @importFrom dplyr bind_rows
filter_to_matches_tokenize <- function(x,
                                       col_raw,
                                       col_ref,
                                       always_tokenize,
                                       token_split,
                                       exclude_freq_raw,
                                       exclude_freq_ref,
                                       exclude_nchar,
                                       exclude_values,
                                       fuzzy,
                                       fuzzy_method,
                                       fuzzy_dist,
                                       is_max_level) {


  ## temp row id
  temp_row_id_x <- "TEMP_ROW_ID_TOKENIZE"
  x[[temp_row_id_x]] <- seq_len(nrow(x))

  if (!always_tokenize) {
    ## find matches that don't require tokenization
    match_initial <- filter_to_matches(
      x,
      col_raw,
      col_ref,
      fuzzy,
      fuzzy_method,
      fuzzy_dist,
      is_max_level,
      return_x = FALSE
    )

    ## separate out matches and non-matches
    x_match <- x[match_initial, , drop = FALSE]
    x_nomatch <- x[!match_initial, , drop = FALSE]
  } else {
    x_match <- NULL
    x_nomatch <- x
  }

  # can_tokenize <- grepl("_", x_nomatch[[col_raw]]) | grepl("_", x_nomatch[[col_ref]])
  # x_nomatch_can_tokenize <- x_nomatch[can_tokenize, , drop = FALSE]

  x_tokenize_match <- filter_to_matches_tokenize_(
    x = x_nomatch,
    col_raw = col_raw,
    col_ref = col_ref,
    col_id = temp_row_id_x,
    token_split = token_split,
    exclude_freq_raw = exclude_freq_raw,
    exclude_freq_ref = exclude_freq_ref,
    exclude_nchar = exclude_nchar,
    exclude_values = exclude_values,
    fuzzy = fuzzy,
    fuzzy_method = fuzzy_method,
    fuzzy_dist = fuzzy_dist,
    is_max_level = is_max_level
  )

  ## combine non-tokenized and tokenized matches
  out <- dplyr::bind_rows(x_match, x_tokenize_match)
  out <- out[order(out[[temp_row_id_x]]), , drop = FALSE]
  row.names(out) <- NULL

  out[,!names(out) %in% temp_row_id_x]
}




#' @noRd
#' @importFrom stringdist stringdist
filter_to_matches_tokenize_ <- function(x,
                                        col_raw,
                                        col_ref,
                                        col_id,
                                        token_split,
                                        exclude_freq_raw,
                                        exclude_freq_ref,
                                        exclude_nchar,
                                        exclude_values,
                                        fuzzy,
                                        fuzzy_method,
                                        fuzzy_dist,
                                        is_max_level) {


  ## prep relevant column names
  by <- c(col_raw, col_ref)
  by_token <- paste0("token_", by)

  col_raw_token <- by_token[1]
  col_ref_token <- by_token[2]

  ## tokenize
  x_token <- tokenize_cols(x, by = by, split = token_split)
  x_token <- unnest_tokens(x_token, by = by_token)

  ## filter to rows with nchar >= exclude_nchar
  # TODO: check whether need to account for NA here
  meets_nchar_col_raw <- nchar(x_token[[col_raw_token]]) >= exclude_nchar
  meets_nchar_col_ref <- nchar(x_token[[col_ref_token]]) >= exclude_nchar
  x_token <- x_token[meets_nchar_col_raw & meets_nchar_col_ref, , drop = FALSE]

  exclude_full_col_raw <- c(exclude_values, exclude_freq_raw)
  exclude_full_col_ref <- c(exclude_values, exclude_freq_ref)

  x_token <- x_token[!x_token[[col_raw_token]] %in% exclude_full_col_raw, , drop = FALSE]
  x_token <- x_token[!x_token[[col_ref_token]] %in% exclude_full_col_ref, , drop = FALSE]

  ## assess tokenized matches
  match_token <- if (fuzzy) {
    stringdist::stringdist(x_token[[col_raw_token]], x_token[[col_ref_token]], method = fuzzy_method) <= fuzzy_dist
  } else {
    x_token[[col_raw_token]] == x_token[[col_ref_token]]
  }

  keep_token <- if (is_max_level) {
    match_token | is.na(x_token[[col_raw_token]]) & is.na(x_token[[col_ref_token]])
  } else {
    match_token | is.na(x_token[[col_raw_token]])
  }

  unique(x_token[keep_token, !names(x_token) %in% by_token, drop = FALSE])
}


#' @noRd
exclude_tokens_freq <- function(x, min_freq) {
  count_tokens(x, min_freq = min_freq, return_values = FALSE)[[1]]
}


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
#' Unlike other `hmatch` functions, the data frame returned by `hmatch_parents`
#' only includes *unique* hierarchical combinations and only relevant
#' hierarchical levels (i.e. the parent level and above), along with additional
#' columns giving the number of matching children and total number of children
#' for a given parent.
#'
#' @inheritParams hmatch
#'
#' @param level name or integer index of the hierarchical level to match at
#'   (i.e. the 'parent' level). If a name, must correspond to a hierarchical
#'   column within `raw`, not including the very last hierarchical column (which
#'   has no hierarchical children). If an integer, must be between 1 and k-1,
#'   where k is the number of hierarchical columns.
#' @param min_matches minimum number of matching offspring required for parents
#'   to be considered a match. Defaults to `1`.
#' @param type type of join ("left", "inner" or "anti") (defaults to "left")
#'
#' @return a data frame obtained by matching the hierarchical columns in `raw`
#'   and `ref` (at the parent level and above), using the join type specified by
#'   argument `type` (see \link{join_types} for more details). Note that unlike
#'   other `hmatch_` functions, hmatch_parents returns only unique rows and
#'   relevant hierarchical columns (i.e. the parent level and above), along with
#'   additional columns describing the number of matching children and total
#'   number of children for a given parent.
#'
#'   \item{...}{hierarchical columns from `raw`, parent level and above}
#'   \item{...}{hierarchical columns from `ref`, parent level and above}
#'   \item{n_child_raw}{total number of unique children belonging to the parent within `raw`}
#'   \item{n_child_ref}{total number of unique children belonging to the parent within `ref`}
#'   \item{n_child_match}{number of children in `raw` with match in `ref`}
#'
#' @examples
#' # e.g. match abbreviated adm1 names to full names based on common offspring
#' raw <- ne_ref
#' raw$adm1[raw$adm1 == "Ontario"] <- "ON"
#' raw$adm1[raw$adm1 == "New York"] <- "NY"
#' raw$adm1[raw$adm1 == "New Jersey"] <- "NJ"
#' raw$adm1[raw$adm1 == "Pennsylvania"] <- "PA"
#'
#' hmatch_parents(
#'   raw,
#'   ne_ref,
#'   pattern = "adm",
#'   level = "adm1",
#'   min_matches = 2,
#'   type = "left"
#' )
#'
#' @importFrom stats aggregate
#' @importFrom dplyr left_join
#' @export hmatch_parents
hmatch_parents <- function(raw,
                           ref,
                           pattern,
                           pattern_ref = pattern,
                           by,
                           by_ref = by,
                           level,
                           min_matches = 1L,
                           type = "left",
                           fuzzy = FALSE,
                           fuzzy_method = "osa",
                           fuzzy_dist = 1L,
                           ref_prefix = "ref_",
                           std_fn = string_std,
                           ...) {


  ## match args
  type <- match.arg(type, c("left", "inner", "anti"))

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

  ## validate argument level
  if (is.character(level)) {
    level <- match(level, prep$by_raw)
    if (is.na(level)) {
      stop("Argument `level` does not match the name of a hierarchical column ",
           "within `raw", call. = FALSE)
    }
  }

  k <- length(prep$by_raw) # number of hierarchical columns

  if (!level %in% seq_len(k-1)) {
    stop("Argument `level` not in range seq_len(k-1), where k is the number ",
         "of hierarchical columns", call. = FALSE)
  }

  ## temp columns
  temp_col_id_raw <- "TEMP_COL_ID_RAW"
  temp_col_id_ref <- "TEMP_COL_ID_REF"
  temp_col_match <- "TEMP_MATCH_COLUMN_PARENTS"

  ## identify parent, offspring, and ancestor columns
  by_raw_parent <- prep$by_raw[level]
  by_ref_parent <- prep$by_ref[level]

  by_raw_child <- prep$by_raw[level + 1L]
  by_ref_child <- prep$by_ref[level + 1L]

  by_raw_ancestors <- prep$by_raw[seq_len(level)]
  by_ref_ancestors <- prep$by_ref[seq_len(level)]

  ## select only the hierarchical columns
  raw_ <- unique(raw[,prep$by_raw, drop = FALSE])
  ref_ <- prep$ref[,prep$by_ref, drop = FALSE]

  ## subset raw and ref to non-missing values at parent and offspring level
  missing_parent_raw <- is.na(raw_[[by_raw_parent]])
  raw_sub <- raw_[!missing_parent_raw, , drop = FALSE]
  raw_sub[[temp_col_id_raw]] <- hcodes_int(raw_sub, by = by_raw_ancestors)

  missing_parent_ref <- is.na(ref_[[by_ref_parent]])
  missing_child_ref <- is.na(ref_[[by_ref_child]])
  ref_sub <- ref_[!missing_parent_ref & !missing_child_ref, , drop = FALSE]
  ref_sub[[temp_col_id_ref]] <- hcodes_int(ref_sub, by = by_ref_ancestors)

  ## main matching routine a child level
  matches_out <- hmatch(
    raw = raw_sub,
    ref = ref_sub,
    by = by_raw_child,
    by_ref = by_ref_child,
    type = "left"
  )

  ## count n_matches, and n_children in both raw and ref, by parent
  n_child_raw <- stats::aggregate(
    list(n_child_raw = raw_sub[[by_raw_child]]),
    raw_sub[,temp_col_id_raw, drop = FALSE],
    function(x) length(unique(x[!is.na(x)]))
  )

  n_child_ref <- stats::aggregate(
    list(n_child_ref = ref_sub[[by_ref_child]]),
    ref_sub[,temp_col_id_ref, drop = FALSE],
    function(x) length(unique(x[!is.na(x)]))
  )

  n_child_match <- stats::aggregate(
    list(n_child_match = matches_out[[1]]),
    matches_out[,c(temp_col_id_raw, temp_col_id_ref), drop = FALSE],
    length
  )

  ## filter to rows with >= min_matches and add temp match col
  n_child_match <- n_child_match[n_child_match$n_child_match >= min_matches,]
  n_child_match[[temp_col_match]] <- rep(TRUE, nrow(n_child_match))

  ## prep raw and ref for join
  raw_join <- unique(raw_sub[, c(by_raw_ancestors, temp_col_id_raw), drop = FALSE])
  ref_join <- unique(ref_sub[, c(by_ref_ancestors, temp_col_id_ref), drop = FALSE])

  ## join raw to all relevant match data
  matches_join_out <- dplyr::left_join(raw_join, n_child_match, by = temp_col_id_raw)
  matches_join_out <- dplyr::left_join(matches_join_out, n_child_raw, by = temp_col_id_raw)
  matches_join_out <- dplyr::left_join(matches_join_out, n_child_ref, by = temp_col_id_ref)
  matches_join_out <- dplyr::left_join(matches_join_out, ref_join, by = temp_col_id_ref)

  ## column order for output
  column_order <- c(
    by_raw_ancestors,
    by_ref_ancestors,
    "n_child_raw",
    "n_child_ref",
    "n_child_match",
    temp_col_id_raw,
    temp_col_match
  )

  ## execute match type and remove temporary columns
  prep_output(
    x = matches_join_out[,column_order],
    type = type,
    temp_col_id = temp_col_id_raw,
    temp_col_match = temp_col_match,
    cols_raw_orig = by_raw_ancestors,
    class_raw = class(raw)
  )
}


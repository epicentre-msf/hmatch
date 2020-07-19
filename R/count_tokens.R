#' Find frequently occurring tokens within a hierarchical column
#'
#' @description
#' Tokenized matching of hierarchical columns can yield false positives when
#' there are tokens that occur frequently in multiple unique hierarchical values
#' (e.g. "South", "North", "City", etc.).
#'
#' This is a helper function to find such frequently-occurring tokens, which can
#' then be passed to the `exclude` argument of \code{\link{hmatch_tokens}}. The
#' frequency calculated is the number of unique,
#' \link[=string_standardization]{string-standardized} values in which a given
#' token is found.
#'
#' @inheritParams hmatch
#'
#' @param x a character vector (generally a hierarchical column)
#' @param split regex pattern used to split values into tokens. By default
#'   splits on any sequence of one or more space characters ("\[:space:\]"),
#'   dashes ("-"), and/or underscores ("_").
#' @param min_freq minimum token frequency (i.e. number of unique values in
#'   which a given token occurs). Defaults to `2`.
#' @param min_nchar minimum token size in number of characters. Defaults to `3`.
#' @param return_values logical indicating whether to return the standardized
#'   values in which each token is found (`TRUE`), or only the count of the
#'   number of unique standardized values (`FALSE`). Defaults to `TRUE`.
#' @param std_fn function to standardize strings, as performed within all
#'   `hmatch_` functions. Defaults to \code{\link{string_std}}. Set to `NULL` to
#'   omit standardization. See also \link{string_standardization}.
#'
#' @examples
#' french_departments <- c(
#'   "Alpes-de-Haute-Provence", "Hautes-Alpes", "Ardennes", "Bouches-du-Rhône",
#'   "Corse-du-Sud", "Haute-Corse", "Haute-Garonne", "Ille-et-Vilaine",
#'   "Haute-Loire", "Hautes-Pyrénées", "Pyrénées-Atlantiques", "Hauts-de-Seine"
#' )
#'
#' count_tokens(french_departments)
#'
#' @importFrom stats aggregate
#' @importFrom dplyr left_join
#' @export count_tokens
count_tokens <- function(x,
                         split = "[-_[:space:]]+",
                         min_freq = 2,
                         min_nchar = 3,
                         return_values = TRUE,
                         std_fn = string_std,
                         ...) {


  ## unique, string-standardized values
  x_unique <- x_unique_std <- unique(x[!is.na(x)])
  if (!is.null(std_fn)) x_unique_std <- std_fn(x_unique_std, ...)

  ## tokenize
  x_token <- tokenize(x_unique, split = split)

  ## map standardized tokens and corresponding standardized value
  x_token_lengths <- lengths(x_token)
  x_token_unlist <- x_token_unlist_std <- unlist(x_token)
  if (!is.null(std_fn)) x_token_unlist_std <- std_fn(x_token_unlist_std, ...)
  x_unique_std_unlist <- rep(x_unique_std, times = x_token_lengths)

  token_df <- unique(
    data.frame(
      token_std = x_token_unlist_std,
      value_std = x_unique_std_unlist,
      stringsAsFactors = FALSE
    )
  )

  ## count n unique standardized values per token
  if (nrow(token_df) > 0) {
    token_std_counts <- stats::aggregate(
      list(n_value_std = token_df$value_std),
      list(token_std = token_df$token_std),
      length
    )

    ## prepare output
    df_tokens_out <- dplyr::left_join(token_df, token_std_counts, by = "token_std")
    df_tokens_out <- df_tokens_out[order(df_tokens_out$n_value_std, decreasing = TRUE),]
    df_tokens_out <- order_within(df_tokens_out, "token_std", "n_value_std")

    ## filter
    df_tokens_out <- df_tokens_out[df_tokens_out$n_value_std >= min_freq,]
    df_tokens_out <- df_tokens_out[nchar(df_tokens_out$token_std) >= min_nchar,]

  } else {
    df_tokens_out <- data.frame(
      token_std = character(0),
      value_std = character(0),
      n_value_std = integer(0),
      stringsAsFactors = FALSE
    )
  }

  ## return values corresponding to each token or simply the count of unique values
  if (return_values) {
    out <- df_tokens_out[,1:2]
  } else {
    out <- unique(df_tokens_out[,c(1, 3)])
  }

  ## remove row names and return
  row.names(out) <- NULL
  out
}

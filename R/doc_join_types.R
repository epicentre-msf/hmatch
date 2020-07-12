#' Types of hierarchical joins
#'
#' @name join_types
#'
#' @description
#' The basic join types used in the \code{hmatch} package ("left", "inner",
#' "anti") are conceptually equivalent to \link{dplyr}'s
#' \code{\link[dplyr]{join}} types.
#'
#' For each of the three join types there is also a counterpart prefixed by
#' "resolve_" ("resolve_left", "resolve_inner", "resolve_anti"). In a resolve
#' join rows of `raw` with matches to multiple rows of `ref` are resolved either
#' to a single best match or no match before the subsequent join type is
#' implemented. In a resolve join, rows of `raw` are never duplicated.
#'
#' The exact details of match resolution vary somewhat among functions, and are
#' explained within each function's documentation.
#'
#' @return \item{left}{return all rows from `raw`, and all columns from `raw`
#' and `ref`. Rows in `raw` with no match in `ref` will have NA values in the
#' new columns taken from `ref`. If there are multiple matches between `raw` and
#' `ref`, all combinations of the matches are returned.}
#'
#' \item{inner}{return only the rows of `raw` that have matches in `ref`, and
#' all columns from `raw` and `ref`. If there are multiple matches between `raw`
#' and `ref`, all combinations of the matches are returned.}
#'
#' \item{anti}{return all rows from `raw` where there are not matches in `ref`,
#' keeping just columns from `raw`}
#'
#' \item{resolve_left}{similar to "left", except that any row of `raw` that
#' initially has multiple matches to `ref` is resolved to either a single 'best'
#' match or no match. All rows of `raw` are returned, and rows of `raw` are
#' never duplicated.}
#'
#' \item{resolve_inner}{similar to "inner", except that any row of `raw` that
#' initially has multiple matches to `ref` is resolved to either a single 'best'
#' match or no match. Only the rows of `raw` that can be resolved to a single
#' best match are returned, and rows of `raw` are never duplicated.}
#'
#' \item{resolve_anti}{similar to "anti", except that any row of `raw` that
#' initially has multiple matches to `ref` is considered non-matching (along
#' with rows of `raw` that initially have no matches to `ref`), and returned as
#' a single row. Rows of `raw` are never duplicated.}
#'
NULL

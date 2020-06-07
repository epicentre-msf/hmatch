#' Types of hierarchical joins
#'
#' @name join_types
#'
#' @description
#' Join types used in the \code{hmatch} package ("left", "inner", "anti") are
#' conceptually equivalent to \link{dplyr}'s \code{\link[dplyr]{join}} types.
#' However, for some functions we also add two new join types ("inner_unique"
#' and "anti_unique"), which are similar to the parent types except that
#' "_unique" joins consider rows in `raw` with \emph{multiple} matches in `ref`
#' as \emph{non}-matches. That is, a match must be unique to count.
#'
#' @return
#' \item{left}{return all rows from `raw`, and all columns from `raw` and `ref`.
#' Rows in `raw` with no match in `ref` will have NA values in the new columns
#' taken from `ref`. If there are multiple matches between `raw` and `ref`, all
#' combinations of the matches are returned.}
#'
#' \item{inner}{return all rows from `raw` where there are matches in `ref`, and
#' all columns from `raw` and `ref`. If there are multiple matches between `raw`
#' and `ref`, all combinations of the matches are returned.}
#'
#' \item{inner_unique}{similar to "inner", except that any row of `raw` with
#' multiple matches in `ref` is considered non-matching and so will not be
#' returned.}
#'
#' \item{anti}{return all rows from `raw` where there are not matches in `ref`,
#' keeping just columns from `raw`}
#'
#' \item{anti_unique}{similar to "anti" except that any row of `raw` with
#' multiple matches in `ref` is considered non-matching, and returned as a
#' single row.}
#'
NULL

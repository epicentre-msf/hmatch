#' Reference dataset
#'
#' Reference table of select administrative districts in the northeastern
#' portion of North America.
#'
#' @format A data.frame with 31 rows and 4 variables, all of class character:
#' \describe{
#' \item{level}{Administrative level}
#' \item{adm0}{Name of administrative 0 level (country)}
#' \item{adm1}{Name of administrative 1 level (state/province)}
#' \item{adm2}{Name of administrative 2 level (county/census division)}
#' \item{hcode}{Hierarchical code}
#' }
"ne_ref"


#' Raw dataset
#'
#' Raw entries of select administrative districts from the northeastern portion
#' of North America.
#'
#' @format A data.frame with 15 rows and 4 variables:
#' \describe{
#' \item{id}{Identifier}
#' \item{adm0}{Name of administrative 0 level (country)}
#' \item{adm1}{Name of administrative 1 level (state/province)}
#' \item{adm2}{Name of administrative 2 level (county/census division)}
#' }
"ne_raw"

#' Reference dataset
#'
#' Reference table of select adminstrative districts from the Democratic
#' Republic of the Congo.
#'
#' @format A data.frame with 15 rows and 6 variables:
#' \describe{
#' \item{level}{Level of geocode}
#' \item{adm1}{Name of administrative 1 level}
#' \item{adm2}{Name of administrative 2 level}
#' \item{adm3}{Name of administrative 3 level}
#' \item{adm4}{Name of administrative 4 level}
#' \item{pcode}{Geocode}
#' }
"drc_ref"



#' Raw dataset
#'
#' Raw entries of adminstrative districts from the Democratic Republic of the
#' Congo.
#'
#' @format A data.frame with 11 rows and 5 variables:
#' \describe{
#' \item{id}{Integer id of entry}
#' \item{adm1}{Name of administrative 1 level}
#' \item{adm2}{Name of administrative 2 level}
#' \item{adm3}{Name of administrative 3 level}
#' \item{adm4}{Name of administrative 4 level}
#' }
"drc_raw"


#' Dataset of manual corrections
#'
#' Raw entries of adminstrative districts from the Democratic Republic of the
#' Congo, and their corresponding geocode.
#'
#' @format A data.frame with 1 row and 5 variables:
#' \describe{
#' \item{adm1}{Name of administrative 1 level}
#' \item{adm2}{Name of administrative 2 level}
#' \item{adm3}{Name of administrative 3 level}
#' \item{adm4}{Name of administrative 4 level}
#' \item{pcode}{Geocode}
#' }
"drc_man"


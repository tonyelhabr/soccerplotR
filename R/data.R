#' Alternate Team Abbreviation Mapping
#'
#' A list of named character vectors mapping common alternate team abbreviations.
#'
#' You can suggest additions to this table by
#' [opening an issue in soccerplotR](https://github.com/tonyelhabr/soccerplotR/issues/new).
#'
#' @examples
#' \donttest{
#' library(purrr)
#'
#' available_countries <- names(soccerplotR::team_name_mapping)
#' available_countries
#' #> [1] "ENG" "ESP" "FRA" "GER" "ITA" "USA"
#'
#' eng_teams <- soccerplotR::team_name_mapping[["ENG"]]
#' eng_teams[c("Wolves", "Wolverhampton Wanderers", "WBA", "West Brom")]
#'
#' all_teams <- flatten_chr(soccerplotR::team_name_mapping)
#' all_teams[c("Tottenham Hotspur", "Tottenham", "AC Milan")]
#' }
#'
#' @format A list of named character vectors, where each element in the list
#'   is an array of teams corresponding to a given country (3-letter ISO code).
#'   Each character vector has the following form:
#' \describe{
#'   \item{name attribute}{The "alternate" name.}
#'   \item{value attribute}{The "correct" name.}
#' }
"team_name_mapping"

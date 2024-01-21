#' Output Valid Soccer Team Names
#'
#' @description Team names used in this function are extracted from Fotmob
#'
#' @param country One of "ENG", "ESP", "FRA", "GER", "ITA", or "USA". (Names are ISO 3166 3-letter country codes.)
#'
#' @return A vector of type `'character'`.
#' @name valid_names
#' @examples
#' all_valid_team_names()
#' valid_team_names("ENG")

#' @rdname valid_names
#' @export
valid_team_names <- function(country) {
  country <- match.arg(country, choices = names(soccerplotR::team_name_mapping))
  sort(unique(soccerplotR::team_name_mapping[[country]]))
}

#' @rdname valid_names
#' @export
all_valid_team_names <- function() {
  countries <- names(soccerplotR::team_name_mapping)

  res <- lapply(countries, function(country) {
    valid_team_names(country)
  })

  names(res) <- countries
  res
}

#' Standardize Soccer Team Names
#'
#' This function standardizes soccer team names to Fotmob defaults.
#'
#' @param team_name a character vector of team names
#' @param keep_non_matches If `TRUE` (the default) an element of `team_name` that can't
#'   be matched to any of the internal mapping vectors will be kept as is. Otherwise
#'   it will be replaced with `NA`.
#'
#' @return A character vector with the length of `team_name` and cleaned team names
#'   if they are included in [`team_name_mapping`], etc.
#'   Non matches may be replaced with `NA` (depending on the value of `keep_non_matches`).
#' @export
#' @examples
#' team_names <- c('Liverpool', 'Brighton', 'Bournemouth', 'AFC Bournemouth')
#'
#' # keep non matches
#' soccerplotR::clean_team_names(team_names, keep_non_matches = TRUE)
clean_team_names <- function(
    team_name,
    keep_non_matches = TRUE
) {
  stopifnot(is.character(team_name))

  mapping_names <- flat_team_name_mapping[team_name]

  if (any(is.na(mapping_names)) && getOption('soccerplotR.verbose', default = interactive())) {
    cli::cli_warn('Abbreviations not found in {.code soccerplotR::team_name_mapping}: {team_name[is.na(mapping_names)]}')
  }

  if (isTRUE(keep_non_matches)) {
    mapping_names <- ifelse(!is.na(mapping_names), mapping_names, team_name)
  }

  mapping_names
}

sanitize_team_name <- function(x) {
  gsub('[.]', '', gsub(' ', '_', x), x)
}

logo_from_team_name <- function(team_name){
  sanitized_team_name <- sanitize_team_name(team_name)
  img_vctr <- paste0(sanitized_team_name, '.png')
  # This used to call the following system.file line
  # but it drops non matches which results in errors
  # system.file(img_vctr, package = 'soccerplotR')

  # Now we use some code from system.file but keep the non matches
  packagePath <- find.package('soccerplotR', quiet = TRUE)
  img_files <- file.path(packagePath, img_vctr)
  present <- file.exists(img_files)
  img_files[!present] <- img_vctr[!present]

  img_files
}

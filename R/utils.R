#' Output Valid Soccer Team Abbreviations
#'
#' @description Thenames used in this function are extracted from Fotmob
#'
#' @param country One of `'ENG'` or `'USA'`
#' @export
#' @return A vector of type `'character'`.
#' @examples
#' valid_team_names('ENG')
#' valid_team_names('USA')
valid_team_names <- function(country = c('ENG', 'USA')){
  country <- rlang::arg_match(country)
  m <- switch (
    country,
    'ENG' = soccerplotR::eng_team_name_mapping
  )
  n <- sort(unique(m))
  n
}

#' Standardize Soccer Team Names
#'
#' This function standardizes soccer team names to Fotmob defaults.
#'
#' @param team_name a character vector ofnames
#' @inheritParams source valid_team_names
#' @param keep_non_matches If `TRUE` (the default) an element of `team_name` that can't
#'   be matched to any of the internal mapping vectors will be kept as is. Otherwise
#'   it will be replaced with `NA`.
#'
#' @return A character vector with the length of `team_name` and cleaned team names
#'   if they are included in [`eng_team_name_mapping`], etc.
#'   (depending on the value of `country`). Non matches may be replaced
#'   with `NA` (depending on the value of `keep_non_matches`).
#' @export
#' @examples
#' team_names <- c('Liverpool', 'Brighton', 'Bournemouth', 'AFC Bournemouth')
#'
#' # keep non matches
#' soccerplotR::clean_team_abbrs(team_names, country = 'ENG', keep_non_matches = TRUE)
#'
#' # replace non matches
#' soccerplotR::clean_team_abbrs(team_names, country = 'ENG', keep_non_matches = FALSE)
clean_team_names <- function(
    team_name,
    country = c('ENG', 'USA'),
    keep_non_matches = TRUE
) {
  stopifnot(is.character(team_name))
  country <- rlang::arg_match(country)

  mapping <- switch (
    country,
    'ENG' = soccerplotR::eng_team_name_mapping
  )

  nms <- unname(mapping[toupper(team_name)])

  if (any(is.na(nms)) && getOption('soccerplotR.verbose', default = interactive())) {
    mapping_note <- switch (
      country,
      'ENG' = 'soccerplotR::eng_team_name_mapping',
      'USA' = 'soccerplotR::usa_team_name_mapping'
    )
    cli::cli_warn('Abbreviations not found in {.code {mapping_note}}: {team_name[is.na(nms)]}')
  }

  if (isTRUE(keep_non_matches)) nms <- ifelse(!is.na(nms), nms, team_name)

  nms
}

logo_from_team_name <- function(team_name, country = c('ENG', 'USA')){
  country <- rlang::arg_match(country)
  img_vctr <- paste0(country, '/', team_name, '.png')
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

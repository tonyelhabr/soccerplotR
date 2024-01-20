io_wrapper <- function(
    f,
    dir,
    id, ## basename
    ext = 'qs',
    name = id,
    read_f = qs::qread,
    write_f = qs::qsave,
    overwrite = FALSE,
    ...
) {

  stopifnot('`id` cannot be missing' = !rlang::is_missing(id))
  stopifnot('`name` cannot be missing' = !rlang::is_missing(name))
  stopifnot('`dir` cannot be missing' = !rlang::is_missing(dir))
  stopifnot('`ext` cannot be missing' = !rlang::is_missing(ext))

  message(sprintf('Scraping %s.', name))

  path <- file.path(dir, paste0(name, '.', ext))

  if (isFALSE(dir.exists(dir))) {
    dir.create(dir, recursive = TRUE)
  }

  if (file.exists(path) & isFALSE(overwrite)) {
    return(read_f(path))
  }
  Sys.sleep(runif(1, 1, 2))

  res <- f(id, ...)
  write_f(res, path)
  res
}

get_fotmob_league_teams <- function(league_id) {
  url <- paste0('https://www.fotmob.com/api/leagues?id=', league_id)
  resp <- httr::GET(url)
  cont <- httr::content(resp, as = 'text')
  result <- jsonlite::fromJSON(cont)
  table_init <- result$table$data
  tables <- dplyr::bind_rows(table_init$table)
  tables$all[[1]] |>
    dplyr::transmute(
      team = name,
      team_id = id
    )
}

get_fotmob_team_colors_logos <- function(team_id) {
  url <- paste0('https://www.fotmob.com/api/teams?id=', team_id)
  resp <- httr::GET(url)
  cont <- httr::content(resp, as = 'text')
  result <- jsonlite::fromJSON(cont)
  list(
    'team_id' = result$details$id,
    'name' = result$details$name,
    'short_name' = result$details$shortName,
    'country' = result$details$country,
    'logo' = result$details$sportsTeamJSONLD$logo,
    'primary' = result$history$teamColorMap$color,
    'secondary' = result$history$teamColorMap$colorAlternate
  )
}

all_leagues <- readr::read_csv('https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/fotmob-leagues/all_leagues.csv')
eng_codes <- all_leagues[all_leagues$ccode == 'ENG', ][1:2, ]
eng_team_standings <- purrr::map_dfr(
  eng_codes$id,
  \(league_id) {
    io_wrapper(
      f = get_fotmob_league_teams,
      id = league_id,
      dir = file.path('data-raw', 'fotmob', 'team_ids', 'ENG')
    )
  }
)

## TODO: nest by country
eng_team_colors_logos <- purrr::map_dfr(
  eng_team_standings$team_id,
  \(team_id) {
    io_wrapper(
      f = get_fotmob_team_colors_logos,
      id = team_id,
      dir = file.path('data-raw', 'fotmob', 'team_details', 'ENG')
    )
  }
)

purrr::walk(
  eng_team_colors_logos$short_name,
  \(short_name) {
    url <- eng_team_colors_logos$logo[eng_team_colors_logos$short_name == short_name]
    dir <- file.path('inst', 'ENG')
    io_wrapper(
      f = \(.x) { .x },
      read_f = \(.x) { .x },
      write_f = \(.x, path) { download.file(.x, destfile = path, mode = 'wb', quiet = TRUE) },
      id = url,
      name = short_name,
      dir = dir,
      ext = 'png'
    )
  }
)

## TODO: Need to combine all country color pals for internal data.
eng_primary_colors <- rlang::set_names(
  eng_team_colors_logos$primary,
  eng_team_colors_logos$short_name
)

eng_secondary_colors <- rlang::set_names(
  eng_team_colors_logos$secondary,
  eng_team_colors_logos$short_name
)

eng_team_name_mapping <- rlang::set_names(
  eng_team_colors_logos$short_name,
  eng_team_colors_logos$short_name
)

# write data ----
usethis::use_data(
  eng_primary_colors,
  eng_secondary_colors,
  internal = TRUE,
  overwrite = TRUE
)

usethis::use_data(
  eng_team_name_mapping,
  internal = FALSE,
  overwrite = TRUE
)

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
  path <- glue::glue(path)

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

get_fotmob_league_teams <- function(league_id, season = 2023) {
  url <- paste0('https://www.fotmob.com/api/leagues?id=', league_id, '&season=', season)
  resp <- httr::GET(url)
  cont <- httr::content(resp, as = 'text')
  result <- jsonlite::fromJSON(cont)
  table_init <- result$table$data
  tables <- dplyr::bind_rows(table_init$table)
  if (any('table' == names(tables))) {
    tables <- tables$table
  }
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

popular_league_ids <- c(47, 54, 87, 53, 130, 55)
tier2_big5_and_mls_ids <- c(48, 110, 146, 86, 140, 8972)
all_league_ids <- c(popular_league_ids, tier2_big5_and_mls_ids)
# all_leagues <- readr::read_csv('https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/fotmob-leagues/all_leagues.csv')

team_standings <- purrr::map_dfr(
  all_league_ids,
  \(league_id) {
    io_wrapper(
      f = get_fotmob_league_teams,
      id = league_id,
      dir = file.path('data-raw', 'fotmob', 'team_ids')
    )
  }
)

team_colors_logos <- purrr::map_dfr(
  team_standings$team_id,
  \(team_id) {
    io_wrapper(
      f = get_fotmob_team_colors_logos,
      id = team_id,
      dir = file.path('data-raw', 'fotmob', 'team_details')
    )
  }
) |>
  dplyr::mutate(
    ## replace non-standard characters
    name = stringi::stri_trans_general(name, "latin-ascii"),
    short_name = stringi::stri_trans_general(short_name, "latin-ascii")
  )

purrr::walk(
  team_colors_logos$short_name,
  \(short_name) {
    url <- team_colors_logos$logo[team_colors_logos$short_name == short_name]
    io_wrapper(
      f = \(.x) { .x },
      read_f = \(.x) { .x },
      write_f = \(.x, path) { download.file(.x, destfile = glue::glue(path), mode = 'wb', quiet = TRUE) },
      id = url,
      name = glue::glue(short_name),
      dir = 'inst',
      ext = 'png'
    )
  }
)

primary_colors <- rlang::set_names(
  team_colors_logos$primary,
  team_colors_logos$short_name
)

secondary_colors <- rlang::set_names(
  team_colors_logos$secondary,
  team_colors_logos$short_name
)

team_name_mapping <- rlang::set_names(
  team_colors_logos$short_name,
  team_colors_logos$short_name
)

# write data ----
usethis::use_data(
  primary_colors,
  secondary_colors,
  internal = TRUE,
  overwrite = TRUE
)

usethis::use_data(
  team_name_mapping,
  internal = FALSE,
  overwrite = TRUE
)

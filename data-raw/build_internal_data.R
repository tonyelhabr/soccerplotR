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

.get_fotmob_league_table_resp <- function(league_id, season) {
  url <- paste0('https://www.fotmob.com/api/leagues?id=', league_id, '&season=', season)
  resp <- httr::GET(url)
  cont <- httr::content(resp, as = 'text')
  result <- jsonlite::fromJSON(cont)
  result$table$data
}

get_fotmob_league_teams <- function(league_id, season = 2023) {
  table_init <- .get_fotmob_league_table_resp(
    league_id = league_id,
    season = season
  )

  nms <- names(table_init)
  cols <- c('all')
  raw_table <- if('table' %in% nms) {
    table_init$table |>
      dplyr::select(dplyr::all_of(cols))
  } else if('tables' %in% nms) {
    tables <- dplyr::bind_rows(table_init$tables)
    tables$all <- tables$table$all
    tables |>
      dplyr::rename(
        group_id = .data[['leagueId']],
        group_page_url = .data[['pageUrl']],
        group_name = .data[['leagueName']]
      ) |>
      dplyr::select(
        -c(.data[['table']], .data[['legend']])
      )
  } else {
    stop(
      'Expected to find `table` or `tables` element but did not.'
    )
  }
  table <- raw_table |>
    tibble::as_tibble() |>
    dplyr::select(dplyr::all_of(cols)) |>
    tidyr::unnest(dplyr::all_of(cols))

  table |>
    dplyr::transmute(
      team = name,
      team_id = id
    )
}

get_fotmob_league_info <- function(league_id, season = 2023) {
  table_init <- .get_fotmob_league_table_resp(
    league_id = league_id,
    season = season
  )
  list(
    league_id = table_init$leagueId,
    league_name = table_init$leagueName,
    country = table_init$ccode
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
    'color_home' = result$history$teamColorMap$color,
    'color_away' = result$history$teamColorMap$colorAway,
    'color_alternate' = result$history$teamColorMap$colorAlternate,
    'color_away_alternate' = result$history$teamColorMap$colorAwayAlternate
  )
}

get_fotmob_league_ids <- function() {
  resp <- httr::POST('https://www.fotmob.com/api/allLeagues')
  cont <- httr::content(resp)

  .extract_leagues <- function(x) {
    cont[[x]] |>
      tibble::enframe() |>
      dplyr::select(dplyr::all_of('value')) |>
      tidyr::unnest_wider(dplyr::all_of('value')) |>
      tidyr::unnest_longer(dplyr::all_of('leagues')) |>
      dplyr::select(dplyr::all_of(c('ccode', 'name', 'leagues'))) |>
      dplyr::rename('country' = 2) |>
      tidyr::unnest_wider(dplyr::all_of('leagues')) |>
      janitor::clean_names() |>
      dplyr::select(dplyr::all_of(c('ccode', 'country', 'id', 'name', 'page_url')))
  }

  purrr::map_dfr(
    c(
      'international',
      'countries'
    ),
    .extract_leagues
  )
}

all_league_ids_df <- get_fotmob_league_ids()
# fotmob:::.fotmob_get_league_tables(77, 'https://fotmob.com/leagues/77/overview/world-cup', 2022)

popular_league_ids <- c(47, 54, 87, 53, 130, 55)
tier2_big5_and_mls_ids <- c(48, 110, 146, 86, 140, 8972)
# international_ids <- 77
international_ids <- all_league_ids_df |>
  dplyr::filter(ccode == 'INT') |>
  # dplyr::filter(name == 'Friendlies') |>
  dplyr::filter(
    name %in% c(
      'World Cup Qualification CONCACAF',
      'World Cup Qualification CONMEBOL',
      'World Cup Qualification UEFA',
      'World Cup Qualification AFC',
      'World Cup Qualification CAF',
      'World Cup Qualification OFC'
    )
  ) |>
  # dplyr::arrange(id)
  dplyr::pull(name, id)
all_league_ids <- c(popular_league_ids, tier2_big5_and_mls_ids, names(international_ids))
# all_leagues <- readr::read_csv('https://raw.githubusercontent.com/JaseZiv/worldfootballR_data/master/raw-data/fotmob-leagues/all_leagues.csv')

league_info <- purrr::map_dfr(
  all_league_ids,
  \(league_id) {
    io_wrapper(
      f = get_fotmob_league_info,
      id = league_id,
      dir = file.path('data-raw', 'fotmob', 'league_info')
    )
  }
)

team_standings <- purrr::map_dfr(
  2018:2023,
  \(season) {
    purrr::map_dfr(
      all_league_ids,
      \(league_id) {
        io_wrapper(
          f = get_fotmob_league_teams,
          id = league_id,
          dir = file.path('data-raw', 'fotmob', 'team_ids', season),
          season = season
        ) |>
          dplyr::mutate(
            'league_id' = league_id,
            'season_id' = season,
            .before = 1
          )
      }
    )
  }
) |>
  dplyr::inner_join(
    league_info |> dplyr::select(league_id, country),
    by = dplyr::join_by(league_id)
  ) |>
  dplyr::arrange(country, team)

team_colors_logos <- purrr::map_dfr(
  sort(unique(team_standings$team_id)),
  \(team_id) {
    io_wrapper(
      f = get_fotmob_team_colors_logos,
      overwrite = FALSE,
      id = team_id,
      dir = file.path('data-raw', 'fotmob', 'team_details')
    )
  }
) |>
  dplyr::select(-country) |>
  dplyr::mutate(
    ## replace non-standard characters since i can't think of a robust way to handle encodings
    name = stringi::stri_trans_general(name, 'latin-ascii'),
    short_name = stringi::stri_trans_general(short_name, 'latin-ascii'),
    primary = color_home,
    secondary = color_away
  ) |>
  dplyr::inner_join(
    team_standings |> dplyr::distinct(team_id, league_id, country),
    by = dplyr::join_by(team_id)
  )

## inst/ files can't have spaces or periods
sanitize_name_for_package <- function(x) {
  gsub('[.]', '', gsub(' ', '_', x), x)
}

purrr::walk(
  team_colors_logos$short_name,
  \(short_name) {
    url <- team_colors_logos$logo[team_colors_logos$short_name == short_name]
    io_wrapper(
      f = \(.x) { .x },
      read_f = \(.x) { .x },
      write_f = \(.x, path) { download.file(.x, destfile = path, mode = 'wb', quiet = TRUE) },
      id = url,
      name = sanitize_name_for_package(short_name),
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

# team_name_mapping <- rlang::set_names(
#   team_colors_logos$short_name,
#   team_colors_logos$short_name
# )

team_name_mapping <- split(
  team_colors_logos,
  team_colors_logos$country
) |>
  purrr::map(
    \(.x) {
      rlang::set_names(
        .x$short_name,
        .x$short_name
      )
    }
  )

team_name_mapping[['ENG']] <- c(
  team_name_mapping[['ENG']],
  ## FBref
  c(
    'AFC Bournemouth' = 'Bournemouth',
    'Brighton & Hove Albion' = 'Brighton',
    'Cardiff City' = 'Cardiff',
    'Huddersfield Town' = 'Huddersfield',
    'Hull City' = 'Hull',
    'Leeds United' = 'Leeds',
    'Leicester City' = 'Leicester',
    'Luton Town' = 'Luton',
    'Manchester City' = 'Man City',
    'Manchester United' = 'Man United',
    'Newcastle United' = 'Newcastle',
    'Norwich City' = 'Norwich',
    'Queens Park Rangers' = 'QPR',
    'Sheffield United' = 'Sheff Utd',
    'Sheffield Wednesday' = 'Sheff Wed',
    'Stoke City' = 'Stoke',
    'Swansea City' = 'Swansea',
    'West Bromwich Albion' = 'West Brom',
    'West Ham United' = 'West Ham',
    # 'Wigan Athletic' = 'Wigan',
    'Wolverhampton Wanderers' = 'Wolves',
    ## Opta
    'WBA' = 'West Brom'
  )
)

countries <- sort(unique(names(team_name_mapping)))
for(country in countries) {
  team_name_mapping[[country]] <- sort(team_name_mapping[[country]])
}

flat_team_name_mapping <- purrr::flatten_chr(team_name_mapping)

# write data ----
usethis::use_data(
  primary_colors,
  secondary_colors,
  flat_team_name_mapping,
  internal = TRUE,
  overwrite = TRUE
)

usethis::use_data(
  team_name_mapping,
  internal = FALSE,
  overwrite = TRUE
)

## Identify conferences
# international_teams <- team_colors_logos |>
#   dplyr::filter(league_id %in% names(international_ids)) |>
#   dplyr::transmute(
#     short_name,
#     continent = international_ids[as.character(league_id)]
#   ) |>
#   dplyr::arrange(short_name)
#
# split(international_teams, international_teams$continent) |>
#   purrr::map(
#     \(.x) {
#       .x$short_name
#     }
#   ) |>
#   purrr::pluck(1) |>
#   datapasta::vector_paste_vertical()

#' ggplot2 Layer for Visualizing Soccer Team Logos
#'
#' @description This geom is used to plot soccer team logos instead
#'   of points in a ggplot. It requires x, y aesthetics as well as a valid ENG
#'   team abbreviation. The latter can be checked with [`valid_team_names()`].
#'
#' @inheritParams ggplot2::geom_point
#' @inheritParams valid_team_names
#' @section Aesthetics:
#' `geom_eng_logos()` understands the following aesthetics (required aesthetics are in bold):
#' \itemize{
#'   \item{**x**}{ - The x-coordinate.}
#'   \item{**y**}{ - The y-coordinate.}
#'   \item{**team_name**}{ - The team abbreviation. Should be one of [`valid_team_names()`]. The function tries to clean team names internally by calling [`clean_team_abbrs()`].}
#'   \item{`alpha = NULL`}{ - The alpha channel, i.e. transparency level, as a numerical value between 0 and 1.}
#'   \item{`colour = NULL`}{ - The image will be colorized with this colour. Use the special character `'b/w'` to set it to black and white. For more information on valid colour names in ggplot2 see <https://ggplot2.tidyverse.org/articles/ggplot2-specs.html?q=colour#colour-and-fill>}
#'   \item{`angle = 0`}{ - The angle of the image as a numerical value between 0° and 360°.}
#'   \item{`hjust = 0.5`}{ - The horizontal adjustment relative to the given x coordinate. Must be a numerical value between 0 and 1.}
#'   \item{`vjust = 0.5`}{ - The vertical adjustment relative to the given y coordinate. Must be a numerical value between 0 and 1.}
#'   \item{`width = 1.0`}{ - The desired width of the image in `npc` (Normalised Parent Coordinates).
#'                           The default value is set to 1.0 which is *big* but it is necessary
#'                           because all used values are computed relative to the default.
#'                           A typical size is `width = 0.075` (see below examples).}
#'   \item{`height = 1.0`}{ - The desired height of the image in `npc` (Normalised Parent Coordinates).
#'                            The default value is set to 1.0 which is *big* but it is necessary
#'                            because all used values are computed relative to the default.
#'                            A typical size is `height = 0.1` (see below examples).}
#' }
#' @param ... Other arguments passed on to [ggplot2::layer()]. These are
#'   often aesthetics, used to set an aesthetic to a fixed value. See the below
#'   section 'Aesthetics' for a full list of possible arguments.
#' @return A ggplot2 layer ([ggplot2::layer()]) that can be added to a plot
#'   created with [ggplot2::ggplot()].
#' @export
#' @examples
#' \donttest{
#' library(soccerplotR)
#' library(ggplot2)
#'
#' team_name <- soccerplotR::valid_team_names()
#'
#' df <- data.frame(
#'   a = rep(1:6, 5),
#'   b = sort(rep(1:5, 6), decreasing = TRUE),
#'   teams = team_name
#' )
#'
#' # keep alpha == 1 for all teams including an 'A'
#' matches <- grepl('A', team_name)
#' df$alpha <- ifelse(matches, 1, 0.2)
#' # also set a custom fill colour for the non 'A' teams
#' df$colour <- ifelse(matches, NA, 'gray')
#'
#' # scatterplot of all logos
#' ggplot(df, aes(x = a, y = b)) +
#'   geom_eng_logos(aes(team_name = teams), width = 0.075) +
#'   geom_label(aes(label = teams), nudge_y = -0.35, alpha = 0.5) +
#'   theme_void()
#'
#' # apply alpha via an aesthetic from inside the dataset `df`
#' # please note that you have to add scale_alpha_identity() to use the alpha
#' # values in your dataset!
#' ggplot(df, aes(x = a, y = b)) +
#'   geom_eng_logos(aes(team_name = teams, alpha = alpha), width = 0.075) +
#'   geom_label(aes(label = teams), nudge_y = -0.35, alpha = 0.5) +
#'   scale_alpha_identity() +
#'   theme_void()
#'
#' # apply alpha and colour via an aesthetic from inside the dataset `df`
#' # please note that you have to add scale_alpha_identity() as well as
#' # scale_color_identity() to use the alpha and colour values in your dataset!
#' ggplot(df, aes(x = a, y = b)) +
#'   geom_eng_logos(aes(team_name = teams, alpha = alpha, colour = colour), width = 0.075) +
#'   geom_label(aes(label = teams), nudge_y = -0.35, alpha = 0.5) +
#'   scale_alpha_identity() +
#'   scale_color_identity() +
#'   theme_void()
#'
#' # apply alpha as constant for all logos
#' ggplot(df, aes(x = a, y = b)) +
#'   geom_eng_logos(aes(team_name = teams), width = 0.075, alpha = 0.6) +
#'   geom_label(aes(label = teams), nudge_y = -0.35, alpha = 0.5) +
#'   theme_void()
#'
#' }
geom_soccer_logos <- function(
    mapping = NULL,
    data = NULL,
    stat = 'identity',
    position = 'identity',
    ...,
    na.rm = FALSE,
    show.legend = FALSE,
    inherit.aes = TRUE
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSoccerlogo,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      country = country,
      ...
    )
  )
}

#' @rdname soccerplotR-package
#' @export
GeomSoccerlogo <- ggplot2::ggproto(
  'GeomSoccerlogo', ggplot2::Geom,
  required_aes = c('x', 'y', 'team_name'),
  # non_missing_aes = c(''),
  default_aes = ggplot2::aes(
    alpha = NULL,
    colour = NULL,
    angle = 0,
    hjust = 0.5,
    vjust = 0.5,
    width = 1.0,
    height = 1.0
  ),
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {

    country <- rlang::arg_match(country)

    team_name <- clean_team_names(
      data$team_name,
      keep_non_matches = TRUE
    )

    data$path <- logo_from_team_name(team_name)

    ggpath::GeomFromPath$draw_panel(
      data = data,
      panel_params = panel_params,
      coord = coord,
      na.rm = na.rm
    )
  },
  draw_key = function(...) grid::nullGrob()
)

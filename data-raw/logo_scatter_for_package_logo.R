library(hexSticker)
library(ggplot2)
library(soccerplotR)
library(showtext)
showtext_auto()

team_names <- unlist(unname(soccerplotR::valid_team_names()))
set.seed(42)
sampled_team_names <- sample(team_names, size = 42)

df <- data.frame(
  a = rep(1:7, 6),
  b = sort(rep(1:6, 7), decreasing = TRUE),
  team_name = sampled_team_names
)

p <- ggplot(df, aes(x = a, y = b)) +
  geom_soccer_logos(aes(team_name = team_name), width = 0.09, alpha = 0.2) +
  coord_cartesian(xlim = c(0.5,7.5), ylim = c(0.5,6.5)) +
  theme_void()


sticker(
  p,
  package = "soccerplotR",
  p_family = "Kanit",
  # p_fontface = "regular",
  p_y = 0.6,
  p_size = 20,
  s_x = 1,
  s_y = 1,
  s_width = 1.9,
  s_height = 1.2,
  spotlight = TRUE,
  l_y = 1.75,
  l_alpha = 0.2,
  l_width = 5,
  h_fill = "#222222",
  h_color = "black",
  h_size = 0.8,
  filename = "data-raw/logo.png",
  u_color = "white",
  u_size = 5,
  dpi = 600
)

usethis::use_logo("data-raw/logo.png")

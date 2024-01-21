library(hexSticker)
library(ggplot2)
library(soccerplotR)
library(showtext)

FONT <- 'Kanit'
sysfonts::font_add_google(FONT)
showtext_auto()

team_names <- unlist(unname(soccerplotR::all_valid_team_names()))
set.seed(42)
sampled_team_names <- sample(team_names, size = 42)

df <- data.frame(
  a = rep(1:7, 6),
  b = sort(rep(1:6, 7), decreasing = TRUE),
  team_name = sampled_team_names
)

# ## https://github.com/tidymodels/TMwR/blob/main/_common.R#L15
# theme_transparent <- function(...) {
#
#   ret <- ggplot2::theme_bw(...)
#
#   trans_rect <- ggplot2::element_rect(fill = "transparent", colour = NA)
#   ret$panel.background  <- trans_rect
#   ret$plot.background   <- trans_rect
#   ret$legend.background <- trans_rect
#   ret$legend.key        <- trans_rect
#
#   ret$legend.position <- "top"
#
#   ret
# }

p <- ggplot(df, aes(x = a, y = b)) +
  geom_soccer_logos(aes(team_name = team_name), width = 0.09, alpha = 0.2) +
  coord_cartesian(xlim = c(0.5,7.5), ylim = c(0.5,6.5)) +
  theme_void()


sticker(
  p,
  package = "soccerplotR",
  p_family = FONT,
  # p_fontface = "regular",
  p_y = 0.8,
  p_size = 40,
  s_x = 1,
  s_y = 1,
  s_width = 1.9,
  s_height = 1.2,
  spotlight = TRUE,
  l_y = 1.75,
  l_alpha = 0.2,
  l_width = 5,
  h_fill = "#60b922",
  h_color = "black",
  h_size = 0.8,
  filename = "data-raw/logo.png",
  u_color = "white",
  u_size = 5,
  dpi = 600
)

usethis::use_logo("data-raw/logo.png")

p <- ggplot(df, aes(x = a, y = b)) +
  geom_soccer_logos(aes(team_name = team_name), width = 0.09, alpha = 0.2) +
  annotate("text", x = 4.5, y = 2.5, label = "soccerplotR", family = "Kanit", size = 7, color = "#ffffff") +
  theme_void() +
  coord_cartesian(xlim = c(0.5,8.5), ylim = c(0.5,4.5))

# ggpreview(p, width = 1280, height = 640, units = "px", dpi = 600,  bg = "#222222")

ggsave(
  "man/figures/social_preview.png",
  p,
  width = 1280,
  height = 640,
  units = "px",
  dpi = 600,
  bg = "#222222"
)

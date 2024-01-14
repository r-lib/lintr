library(hexSticker)
library(magick)
library(showtext)
library(sysfonts)

# Loading Google fonts (http://www.google.com/fonts)
google_font_name <- "IBM Plex Serif"
font_add_google(google_font_name)

# Automatically use showtext to render text for future devices
showtext_auto()

project_root <- here::here()

# https://www.flaticon.com/free-icon/lint-roller_6981024
image <- image_read(file.path(project_root, "inst", "hextools", "lint-roller.png"))
manual_logo_path <- file.path(project_root, "man", "figures", "logo.png")

sticker(
  # image
  subplot = image,
  s_x = 1,
  s_y = 0.85,
  s_width = 0.9,
  s_height = 0.9,
  # package name
  package = "lintr",
  p_color = "#545452",
  p_family = google_font_name,
  p_size = 45,
  p_x = 1,
  p_y = 1.55,
  # border
  h_color = "grey",
  h_fill = "white",
  # url
  url = "       https://lintr.r-lib.org/",
  u_size = 10,
  u_color = "grey",
  # saving sticker
  dpi = 600,
  filename = manual_logo_path
)

pkgdown::build_favicons()

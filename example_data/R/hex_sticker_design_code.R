# load the necessary packages
library(hexSticker) # hexSticker generator
library(magick)     # Advanced image processing
library(sysfonts)   # font selection
library(tidyverse)

# Sticker function--------------------------- 

sticker(
  subplot, #              * image/ggplot object
  s_x = 0.8, #            * subplot x-position (left/right position)
  s_y = 0.75, #           * subplot y-position (up/down position)
  s_width = 0.4, #        * subplot width
  s_height = 0.5, #       * subplot height
  package, #              * package name to be displayed in hexSticker
  p_x = 1, #              * package name x-position
  p_y = 1.4, #            * package name y-position
  p_color = "#FFFFFF", #  * package name color
  p_family = "Aller_Rg", #* package name font family
  p_fontface = "plain", # * package name font face
  p_size = 8, #           * package name font size
  h_size = 1.2, #         * hexSticker size
  h_fill = "#1881C2", #   * hexSticker background color
  h_color = "#87B13F", #  * hexsticker border color
  spotlight = FALSE, #    * add spotlight effect to hexSticker
  l_x = 1, #              * spotlight effect x-position
  l_y = 0.5, #            * spotlight effect y-position
  l_width = 3, #          * spotlight effect width
  l_height = 3, #         * spotlight effect height
  l_alpha = 0.4, #        * spotlight effect level
  url = "", #             * url to add to the hexSticker
  u_x = 1, #              * url x-position
  u_y = 0.08, #           * url y-position
  u_color = "black", #    * url font color
  u_family = "Aller_Rg", #* url font family
  u_size = 1.5, #         * url font size
  u_angle = 30, #         * url angle
  white_around_sticker = FALSE, # * add white blocks around sticker
  filename = paste0(package, ".png"), # * save hexSticker to file
  asp = 1, # * adjust aspect ratio
  dpi = 300 # * Dots Per Inch resolution
)

# Create your first sticker------------------
pine_img <- image_read('pine.png')

fonts_dataset <- font_files()

font_add("Old English", "OLDENGL.TTF")

sticker(
  subplot = pine_img,
  package = "Pineapple",
  s_width = 1,
  s_height = 1,
  s_x = 1,
  s_y = 0.75,
  p_size = 20,
  h_fill = 'gold',
  h_color = 'hotpink',
  h_size = 1.5,
  url = "www.pineapples2go.com",
  u_size = 5,
  u_color = 'violetred',
  spotlight = T,
  l_y = 1,
  l_x = 1,
  l_width = 3,
  l_height = 3,
  l_alpha = 0.3,
  p_color = 'purple',
  p_family = "Old English"
) %>% print()
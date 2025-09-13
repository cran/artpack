## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center"
)

## ----ex-1---------------------------------------------------------------------

#| fig-alt: "Scatter plot on dark background showing various colored circular points and concentric spiral patterns. Points include solid circles in blue, teal, purple, green, and orange, along with larger spiral/circular patterns in magenta, purple, and orange-red. The elements are randomly distributed across the plot area."

# install.packages("artpack")
library(ggplot2)
library(artpack)
set.seed(0515)

df_packed_circles <-
   packer(
   n = 25, big_r = 7, med_r = 3, small_r = 1,
   min_x = 0, max_x = 100, min_y = 0, max_y = 100,
   color_pal = art_pals("rainbow", 15),
   circle_type = "swirl"
 )
 
 df_packed_circles |>
   ggplot(aes(x, y, group = group, color = I(color))) + # I (base R) takes the color values as-is and passes it through
   theme_void() +
   theme(plot.background = element_rect(fill = "#333333")) +
   geom_path() +
   coord_equal()


## ----ex-2---------------------------------------------------------------------

#| fig-alt: "Scatter plot on dark background showing various colored circular points and concentric spiral patterns. Points include solid circles in blue, teal, purple, green, and orange, along with larger spiral/circular patterns in magenta, purple, and orange-red. The elements are randomly distributed across the plot area."
#| 
df_packed_circles |>
  ggplot(aes(x, y, group = group, color = color)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#333333")) +
  scale_color_identity(guide = "none") + # Takes color values as-is and removes plot legend
  geom_path() +
  coord_equal()


## ----ex-3---------------------------------------------------------------------

#| fig-alt: "Similar scatter plot to previous with slight variations in positioning. Shows colored circular points and concentric spiral patterns on dark background, with solid circles in various colors (blue, teal, purple, green, orange) and larger decorative spiral elements in magenta, purple, and orange-red."
#| 
df_packed_circles |>
  ggplot(aes(x, y, group = group)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#333333")) +
  geom_path(aes(x = x + 1, y = y - .7), color = "#000000") + # Changes made directly in the layer
  geom_path(color = df_packed_circles$color) + # Column from df placed directly in the layer
  coord_equal()


## ----ex-4---------------------------------------------------------------------

#| fig-alt: "Heat map or grid visualization with color gradient from yellow at bottom to bright magenta/pink at top. The grid appears to be approximately 12x12 squares with smooth color transitions creating a temperature or intensity visualization."
#| 
df_grid <-
  grid_maker(
    xlim = c(0,10),
    ylim = c(0,10),
    size = 10,
    fill_pal = art_pals("sunnyside", 5),
    color_pal = sapply(art_pals("sunnyside", 5), \(x) set_brightness(x, 0.40))
  )

df_grid |>
  ggplot(aes(x,y, group = group)) +
  geom_polygon(
    fill = df_grid$fill,
    color = df_grid$color
  ) +
  theme_void() +
  coord_equal()


## ----ex-5---------------------------------------------------------------------

#| fig-alt: "Grid-based visualization on yellow background showing a diagonal white triangular or wedge pattern. The triangle extends from the bottom-left corner toward the upper-right, creating a clear geometric division in the square grid layout."
#| 
df_grid |>
  ggplot(aes(x,y)) +
  geom_polygon(
    fill = df_grid$fill,
    color = df_grid$color
  ) +
  theme_void() +
  coord_equal()


## ----ex-6---------------------------------------------------------------------

#| fig-alt: "Pixelated or mosaic-style visualization with irregular white and colored blocks. Uses a gradient color scheme from yellow at bottom through orange and red to magenta at top, creating an abstract pattern with white negative spaces throughout."
#| 
set.seed(01234)

df_grid |>
  group_sample(group = group, prop = .70) |>
  ggplot(aes(x,y, group = group, fill = I(fill), color = I(color))) +
  geom_polygon() +
  theme_void() +
  coord_equal()



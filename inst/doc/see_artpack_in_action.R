## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 7
)

## ----data-load, message=FALSE, warning=FALSE----------------------------------
# Library Load in-------
library(artpack)
library(tibble)
library(dplyr)
library(purrr)
library(ggplot2)

# Set canvas size#
x_lim <- c(0, 100)
y_lim <- x_lim

# Set seed for reproducibility
set.seed(1000)

## ----sky, fig.align='center'--------------------------------------------------

#| fig.alt: >
#|   Gradient sky background showing a smooth color transition from orange 
#|   at the bottom to light blue at the top, created using horizontal bars 
#|   with the artpack beach color palette.

# Sky Specs----
n_sky_bars <- 100
vec_sky_colors <- art_pals("beach", n_sky_bars)
df_sky_bars <-
  tibble(
    x = x_lim[1], # Horizontal bars means the x value is constant
    xend = x_lim[2], # Horizontal bars means the xend values is constant
    y = seq(y_lim[1], y_lim[2], length = n_sky_bars),
    yend = y, # Horizontal bars means yend is the same as y
    color = vec_sky_colors
  )

# Start the ggplot
df_sky_bars |> 
  ggplot(aes(x,y)) +
  geom_segment(
    aes(xend = xend, yend = yend),
    color = df_sky_bars$color,
    linewidth = 4 #Tweak the bar thickness here if needed.
    ) +
  coord_equal(xlim = x_lim, ylim = y_lim, expand = FALSE)


## ----packer, fig.align='center'-----------------------------------------------

#| fig.alt: >
#|   The same gradient sky background from orange to blue, now with a 
#|   subtle white swirl pattern overlay added using artpack's packer 
#|   function. The overlay creates thin, semi-transparent white curved 
#|   lines that add texture and depth to the sky gradient.

# Make the data
df_overlay <-
  packer(
    n = 300,
    min_x = x_lim[1],
    max_x = x_lim[2],
    min_y = y_lim[1],
    max_y = y_lim[2],
    color_pal = "#F5E0CD",
    circle_type = "swirl"
  )

# And add it to the plot
df_sky_bars |>
  ggplot(aes(x,y)) +
  geom_segment(
    aes(xend = xend, yend = yend),
    color = df_sky_bars$color,
    linewidth = 4
  ) +
  geom_path(
    data = df_overlay,
    aes(group = group),
    color = "#ffffff",
    linewidth = .3,
    alpha = .09
  ) +
  coord_equal(xlim = x_lim, ylim = y_lim, expand = FALSE)


## ----sun_rays, fig.align='center'---------------------------------------------

#| fig.alt: >
#|   The gradient sky with overlay, now featuring radiating sun rays. 
#|   Multiple triangular light-colored rays emanate from a central point 
#|   at coordinates (50, 10), creating a classic sunburst pattern across 
#|   the upper portion of the image. The rays were created using artpack's 
#|   rotator function to rotate and repeat a single triangle shape.

# Sun rays Specs----
n_sun_rays <- 50
# The rays angles
vec_ray_angles <- seq(0, 180, length = n_sun_rays)
# The ray groups
vec_ray_groups <- group_numbers(vec_ray_angles, prefix = "ray", sep = "_")
# The fill colors
vec_ray_fills <- rep_along(vec_ray_groups, c("#F5E0CD", NA))
# Data for One sun ray
# using a tribble for manual shapes can be helpful to keep the correct coords together
df_sun_ray <-
  tribble(
    ~x, ~y,
    50, 10, # Where the center of the sun will be
    175, 10, #trial and error arbitrary x coord that covers the image fully
    175, 20,
    50, 10
  )

# Ray options to iterate through
lst_ray_opts <-
  list(
    vec_ray_angles,
    vec_ray_groups,
    vec_ray_fills
  )

# Create the data frame
# Note that purrr is this artist's preferred method of iteration, but you do you!
# You want those for-loops?? go ahead and get loopy!
df_sun_rays <-
  pmap(lst_ray_opts,
       ~df_sun_ray |>
         rotator(x, y, angle = ..1, anchor = c(50, 10)) |>
         mutate(
           group = ..2,
           fill = ..3
         )
  ) |>
  list_rbind()


# Add to the plot
df_sky_bars |>
  ggplot(aes(x,y)) +
  geom_segment(
    aes(xend = xend, yend = yend),
    color = df_sky_bars$color,
    linewidth = 4
  ) +
  geom_path(
    data = df_overlay,
    aes(group = group),
    color = "#ffffff",
    linewidth = .3,
    alpha = .09
  ) +
  geom_polygon(
    data = df_sun_rays,
    aes(group = group),
    fill = df_sun_rays$fill,
    alpha = .3
  ) +
  coord_equal(xlim = x_lim, ylim = y_lim, expand = FALSE)


## ----sun, fig.align='center'--------------------------------------------------

#| fig.alt: >
#|   The layered artwork now includes a bright orange-yellow circular sun 
#|   positioned at the center of the radiating sun rays. The sun was created 
#|   using artpack's circle_data function and appears as a filled circle 
#|   with a subtle light border that blends with the sun rays behind it.

# Make a circle----
df_sun <-
  circle_data(
    x = 50,
    y = 10,
    r = 25,
    color = "#F5E0CD",
    fill = "#FAB649"
  )

# And add the two layers to the plot
df_sky_bars |>
  ggplot(aes(x,y)) +
  geom_segment(
    aes(xend = xend, yend = yend),
    color = df_sky_bars$color,
    linewidth = 4
  ) +
  geom_path(
    data = df_overlay,
    aes(group = group),
    color = "#ffffff",
    linewidth = .3,
    alpha = .09
  ) +
  geom_polygon(
    data = df_sun_rays,
    aes(group = group),
    fill = df_sun_rays$fill,
    alpha = .3
  ) +
  geom_path(
    data = df_sun,
    color = df_sun$color,
    alpha = .1,
    linewidth = 2
  ) +
  geom_polygon(
    data = df_sun,
    fill = df_sun$fill
  ) +
  coord_equal(xlim = x_lim, ylim = y_lim, expand = FALSE)


## ----waves, fig.align='center'------------------------------------------------

#| fig.alt: >
#|   The completed vintage sunset artwork featuring all five layers: a 
#|   gradient sky from orange to blue, white swirl overlay texture, 
#|   radiating sun rays, a bright orange sun, and stylized water waves 
#|   in the foreground. The waves appear as layered, semi-transparent 
#|   teal and blue curved shapes created using artpack's wave_data 
#|   function, completing the retro sunset scene. The final image has 
#|   a clean presentation with axes and labels removed.

# Wave specs----
# Number of waves
n_waves <- 400
# Starting points of the waves (randomized)
vec_wave_starts <- sample(seq(-75,75, l = 50), n_waves, replace = TRUE)
# Length of waves (randomized)
vec_wave_lengths <- vec_wave_starts + sample(seq(25,100, l = 30), n_waves, replace = TRUE)
# Sizes of waves (randomized)
vec_wave_sizes <- sample(seq(1, 3, l = 20), n_waves, replace = TRUE)
# frequency (humps) of each wave (randomized)
vec_wave_freqs <- sample(1:5, n_waves, replace = TRUE)
# Fill colors of waves
vec_wave_fills <- art_pals("ocean", n_waves, randomize = TRUE)
# Dampening of each wave
vec_wave_damps <- sample(seq(3,10, l = 10), n_waves, replace = TRUE)
# Group Numbering for each wave
vec_wave_groups <- group_numbers(1:n_waves)
# Wave Y transformations (randomized)
vec_wave_trans <- sample(seq(-5,9, l = 20), n_waves, replace = TRUE)

# Make the spec list
lst_wave_opts <-
  list(
    vec_wave_starts, # ..1
    vec_wave_lengths, #..2
    vec_wave_sizes, #..3
    vec_wave_freqs, #..4
    vec_wave_fills, #..5
    vec_wave_damps, #..6
    vec_wave_groups, #..7
    vec_wave_trans #..8
  )

# Make the data#
df_waves <-
  pmap(lst_wave_opts,
       ~wave_data(
         start = ..1,
         end = ..2, 
         size = ..3,
         freq = ..4,
         fill = ..5,
         dampen = ..6,
         group_var = TRUE
       ) |>
         mutate(
           group = paste0(..7),
           y = y + ..8)
  ) |>
  list_rbind()

# And add it to the plot
df_sky_bars |>
  ggplot(aes(x,y)) +
  geom_segment(
    aes(xend = xend, yend = yend),
    color = df_sky_bars$color,
    linewidth = 4
  ) +
  geom_path(
    data = df_overlay,
    aes(group = group),
    color = "#ffffff",
    linewidth = .3,
    alpha = .09
  ) +
  geom_polygon(
    data = df_sun_rays,
    aes(group = group),
    fill = df_sun_rays$fill,
    alpha = .3
  ) +
  geom_path(
    data = df_sun,
    color = df_sun$color,
    alpha = .1,
    linewidth = 2
  ) +
  geom_polygon(
    data = df_sun,
    fill = df_sun$fill
  ) +
  geom_polygon(
    data = df_waves,
    aes(group = group),
    fill = df_waves$fill,
    alpha = .5,
    position = position_jitter(width = .1, height = .02)
  ) +
  coord_equal(xlim = x_lim, ylim = y_lim, expand = FALSE) +
  theme_void()



## ----saving, eval = FALSE-----------------------------------------------------
# # Use ggplot2 to save it locally if you'd like:
# ggsave("vintage_sun.png", dpi = 300, bg = "transparent")


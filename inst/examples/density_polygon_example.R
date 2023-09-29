library(densityarea)
library(dplyr)
library(purrr)
library(sf)

ggplot2_inst <- require(ggplot2)
tidyr_inst <- require(tidyr)

set.seed(10)
x <- c(rnorm(100))
y <- c(rnorm(100))

# ordinary data frame output
poly_df <- density_polygons(x,
                            y,
                            probs = ppoints(5))

head(poly_df)

# It's necessary to specify a grouping factor that combines `level_id` and `id`
# for cases of multimodal density distributions
if(ggplot2_inst){
  ggplot(poly_df, aes(x, y)) +
    geom_path(aes(group = paste0(level_id, id),
                  color = prob))
}

# sf output
poly_sf <- density_polygons(x,
                            y,
                            probs = ppoints(5),
                            as_sf = TRUE)

head(poly_sf)

# `geom_sf()` is from the `{sf}` package.
if(ggplot2_inst){
  poly_sf |>
    arrange(desc(prob)) |>
    ggplot() +
    geom_sf(aes(fill = prob))
}

# Tidyverse usage

data(s01)

# Data transformation
s01 <- s01 |>
  mutate(log_F1 = -log(F1),
         log_F2 = -log(F2))

## Basic usage with `dplyr::reframe()`
### Data frame output
s01 |>
  group_by(name) |>
  reframe(density_polygons(log_F2,
                           log_F1,
                           probs = ppoints(5))) ->
  speaker_poly_df

if(ggplot2_inst){
  speaker_poly_df |>
    ggplot(aes(log_F2, log_F1)) +
    geom_path(aes(group = paste0(level_id, id),
                  color = prob)) +
    coord_fixed()
}

### sf output
s01 |>
  group_by(name) |>
  reframe(density_polygons(log_F2,
                           log_F1,
                           probs = ppoints(5),
                           as_sf = TRUE)) |>
  st_sf() ->
  speaker_poly_sf

if(ggplot2_inst){
  speaker_poly_sf |>
    ggplot() +
    geom_sf(aes(color = prob),
            fill = NA)
}

## basic usage with dplyr::summarise()
### data frame output

if(tidyr_inst){
  s01 |>
    group_by(name) |>
    summarise(poly = density_polygons(log_F2,
                                      log_F1,
                                      probs = ppoints(5),
                                      as_list = TRUE)) |>
    unnest(poly) ->
    speaker_poly_df
}
### sf output

if(tidyr_inst){
  s01 |>
    group_by(name) |>
    summarise(poly = density_polygons(
      log_F2,
      log_F1,
      probs = ppoints(5),
      as_list = TRUE,
      as_sf = TRUE
    )) |>
    unnest(poly) |>
    st_sf() ->
    speaker_poly_sf
}

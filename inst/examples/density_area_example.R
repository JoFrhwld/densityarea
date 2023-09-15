library(densityarea)
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)

# basic usage

set.seed(10)
x <- rnorm(100)
y <- rnorm(100)

density_area(x,
             y,
             probs = ppoints(50),
             as_list = FALSE) ->
  poly_areas_df

head(poly_areas_df)

# Plotting the relationship between probability level and area
ggplot(poly_areas_df,
       aes(prob, area))+
  geom_line()

# Assuming distribution is circular, the radius would be `sqrt(area/pi).`
poly_areas_df |>
  mutate(
    radius = sqrt(area/pi)
  ) |>
  ggplot(aes(prob, radius)) +
    geom_line()

# Tidyverse usage

data(s01)

## Data preprocessing

s01 |>
  mutate(log_F2 = -log(F2),
         log_F1 = -log(F1))->
  s01

s01 |>
  group_by(name) |>
  summarise(
    area_df = density_area(log_F2,
                           log_F1,
                           probs = ppoints(10),
                           as_sf = FALSE,
                           n = 200),
    area_sf = density_area(log_F2,
                           log_F1,
                           probs = ppoints(10),
                           as_sf = TRUE,
                           n = 200)
  ) ->
  s01_areas

s01_areas |>
  unnest(area_df) |>
  ggplot(
    aes(prob, area)
  )+
    geom_line()

# If the shape were an equilateral triangle, each side would be
# equal to `sqrt(area * (4/sqrt(3)))`

s01_areas |>
  unnest(
    area_sf
  ) |>
  st_sf() |>
  arrange(desc(prob)) |>
  mutate(side = sqrt(area * (4/sqrt(3)))) |>
  ggplot()+
    geom_sf(aes(fill = side))

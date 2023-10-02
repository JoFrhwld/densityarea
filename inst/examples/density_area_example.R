library(densityarea)
library(dplyr)
library(sf)

ggplot2_inst <- require(ggplot2)

# basic usage

set.seed(10)
x <- rnorm(100)
y <- rnorm(100)

density_area(x,
             y,
             probs = ppoints(50)) ->
  poly_areas_df

head(poly_areas_df)

# Plotting the relationship between probability level and area
if(ggplot2_inst){
  ggplot(poly_areas_df,
         aes(prob, area)) +
    geom_line()
}

# Tidyverse usage

data(s01)

## Data preprocessing

s01 |>
  mutate(log_F2 = -log(F2),
         log_F1 = -log(F1)) ->
  s01

### Data frame output

s01 |>
  group_by(name) |>
  reframe(density_area(log_F2,
                       log_F1,
                       probs = ppoints(10))) ->
  s01_areas_df

if(ggplot2_inst){
  s01_areas_df |>
    ggplot(aes(prob, area)) +
    geom_line()
}

### Including sf output

s01 |>
  group_by(name) |>
  reframe(density_area(log_F2,
                       log_F1,
                       probs = ppoints(10),
                       as_sf = TRUE)) |>
  st_sf() ->
  s01_areas_sf

if(ggplot2_inst){
  s01_areas_sf |>
    arrange(desc(prob)) |>
    ggplot() +
    geom_sf(aes(fill = area))
}

#' get isobands
#'
#' @param x
#' @param y
#' @param probs
#' @param ...
#'
#' @export
get_isolines<- function(x,
                         y,
                         probs = 0.5,
                         ...) {

  rangex = ((range(x) * 0.25) * c(-1, 1)) + range(x)
  rangey = ((range(y) * 0.5) * c(-1, 1)) + range(y)
  tibble::tibble(x = x,
                 y = y) |>
    ggdensity::get_hdr(probs = probs,
                       rangex = rangex,
                       rangey = rangey,
                       ...) ->
    density_estimate

  density_estimate$df_est$z <- density_estimate$df_est$fhat

  isolines <- xyz_to_isolines(density_estimate$df_est,
                              density_estimate$breaks)

  isolines |>
    purrr::map(tibble::as_tibble) |>
    purrr::list_rbind(names_to = "line") ->
    isolines_df

  return(isolines_df)
}


#' Density polygons
#'
#' @description
#' Given numeric vectors `x` and `y`, `density_polygons()` will return
#' a data frame, or list of a data frames, of the polygon defining 2d kernel
#' densities.
#'
#' @details
#' When using `density_polygons()` together with tidyverse verbs, like
#' [dplyr::summarise()], `as_list` should be `TRUE`.
#'
#'
#' @param x,y Numeric data dimensions
#' @param probs Probabilities to compute density polygons for
#' @param as_sf Should the returned values be [sf::sf]? Defaults to `FALSE`.
#' @param as_list Should the returned value be a list? Defaults to `TRUE` to
#' work well with tidyverse list columns
#' @param ... Additional arguments to be passed to [ggdensity::get_hdr()]
#'
#' @returns A list of data frames, if `as_list=TRUE`, or just a data frame,
#' if `as_list=FALSE`
#'
#'
#'
#' @export

density_polygons <- function(x,
                             y,
                             probs = 0.5,
                             as_sf = FALSE,
                             as_list = TRUE,
                             ...) {
  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  nameswap <- c("x", "y")
  names(nameswap) <- c(xname, yname)

  isolines <- get_isolines(x, y, probs, ...)

  isolines |>
    dplyr::mutate(
      line_id = line |>
        as.numeric() |>
        dplyr::desc() |>
        as.factor() |>
        as.numeric(),
      prob = sort(probs)[line_id],
      order = dplyr::row_number()
    ) |>
    dplyr::select(-line) |>
    dplyr::select(line_id,
           id,
           prob,
           x,
           y,
           order) |>
    dplyr::rename(any_of(nameswap)) ->
    iso_poly_df

  if (!as_sf & as_list) {
    return(list(iso_poly_df))
  } else if (!as_sf) {
    return(iso_poly_df)
  }

  iso_poly_df |>
    dplyr::mutate(
      polygon_id = paste(line_id, id, sep = "-")
    ) |>
    sfheaders::sf_polygon(
      x = "x",
      y = "y",
      polygon_id = "polygon_id",
      keep = T
    ) |>
    dplyr::select(-polygon_id) |>
    dplyr::group_by(
      line_id, prob
    ) |>
    dplyr::summarise() ->
    iso_poly_st

  if (as_list) {
    return(list(iso_poly_st))
  } else{
    return(iso_poly_st)
  }
}


#' Density Area
#' @export

density_area <- function(x,
                         y,
                         probs = 0.5,
                         as_sf = F,
                         as_list = T,
                         ...) {
  density_polygons(
    x = x,
    y = y,
    probs = probs,
    as_sf = T,
    as_list = T,
    ...
  ) ->
    iso_poly_st

  iso_poly_st |>
    purrr::map(\(x) x |>
          dplyr::mutate(area = sf::st_area(geometry))) -> area_poly

  if (!as_sf) {
    area_poly |>
      purrr::map(\(x) x |> sf::st_drop_geometry()) ->
      area_poly
  }

  if (!as_list) {
    area_poly <- area_poly |>
      purrr::list_rbind()
  }

  return(area_poly)
}

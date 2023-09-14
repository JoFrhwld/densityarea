#' get isobands
#'
#' @param x
#' @param y
#' @param probs
#' @param ...
#'
#' @export
get_isobands <- function(x,
                         y,
                         probs = 0.5,
                         ...) {
  tibble::tibble(x = x,
                 y = y) |>
    ggdensity::get_hdr(probs = probs,
                       ...) ->
    density_estimate

  density_estimate$df_est$z <- density_estimate$df_est$fhat

  isobands <- xyz_to_isobands(density_estimate$df_est,
                              density_estimate$breaks)

  isobands |>
    purrr::map(tibble::as_tibble) |>
    purrr::list_rbind(names_to = "band") ->
    isobands_df

  return(isobands_df)
}


#' Density polygons
#'
#' Given numeric vectors \code{x} and \{y}, \code{density_polygons} will return
#' a dataframe, or list of a dataframe, of the polygon defining 2d kernel
#' densities
#'
#' @param x
#' @param y
#' @param probs
#' @param as_sf
#' @param as_list
#' @param ...
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

  isobands <- get_isobands(x, y, probs, ...)

  isobands |>
    tidyr::separate(
      band,
      into = c("start", "end"),
      sep = ":",
      convert = T
    ) |>
    dplyr::mutate(
      band_id = start |>
        dplyr::desc() |>
        as.factor() |>
        as.numeric(),
      prob = sort(probs)[band_id],
      order = dplyr::row_number()
    ) |>
    dplyr::select(-start,-end) |>
    dplyr::select(band_id,
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
    sf::st_as_sf(coords = c(xname, yname))  |>
    dplyr::group_by(band_id, id, prob) |>
    dplyr::summarise() |>
    sf::st_cast("POLYGON") |>
    sf::st_convex_hull() |>
    dplyr::group_by(band_id, prob) |>
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
                         drop_geometry = T,
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

  if (drop_geometry) {
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

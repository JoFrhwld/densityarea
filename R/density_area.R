#' Density Area
#' @export

density_area <- function(x, y, probs = 0.5, ...){

  tibble::tibble(
    x = x,
    y = y
  ) |>
    ggdensity::get_hdr(
      probs = probs,
      ...
    ) ->
    density_estimate

  density_estimate$df_est$z <- density_estimate$df_est$fhat

  isobands <- xyz_to_isobands(
    density_estimate$df_est,
    density_estimate$breaks
  )

  isobands |>
    purrr::map(
      tibble::as_tibble
    ) |>
    purrr::list_rbind(
      names_to = "band"
    ) ->
    iso_poly

  iso_poly |>
    sf::st_as_sf(
      coords = c("x", "y")
    )  |>
    dplyr::group_by(
      band, id
    ) |>
    dplyr::summarise() |>
    sf::st_cast("POLYGON") |>
    sf::st_convex_hull() |>
    dplyr::group_by(
      band
    ) |>
    dplyr::summarise() |>
    dplyr::mutate(
      area = sf::st_area(geometry)
    ) |>
    tidyr::separate(
      band,
      into = c("start", "end"),
      sep = ":",
      convert = T
    ) |>
    dplyr::arrange(
      dplyr::desc(start)
    ) |>
    dplyr::mutate(prob = probs) |>
    sf::st_drop_geometry() |>
    select(
      area,
      prob
    ) ->
    area_poly

  if(nrow(area_poly) == 1){
    return(area_poly$area)
  } else {
    return(area_poly)
  }

  return(area_poly)
}

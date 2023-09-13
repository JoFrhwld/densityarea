#' Density polygons
#' @export

density_polygons <- function(
    x,
    y,
    probs = 0.5,
    as_st = FALSE,
    ...
){
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
    ) |>
    tidyr::separate(
      band,
      into = c("start", "end"),
      sep = ":",
      convert = T
    ) |>
    mutate(
      band_id = start |>
        desc() |>
        as.factor() |>
        as.numeric(),
      prob = sort(probs)[band_id],
      order = row_number()
    ) |>
    select(-start, -end) |>
    select(
      band_id,
      id,
      prob,
      x,
      y,
      order
    )->
    iso_poly_df

  if(!as_st){
    return(list(iso_poly_df))
  }

  iso_poly_df |>
    sf::st_as_sf(
      coords = c("x", "y")
    )  |>
    dplyr::group_by(
      band_id, id, prob
    ) |>
    dplyr::summarise() |>
    sf::st_cast("POLYGON") |>
    sf::st_convex_hull() |>
    dplyr::group_by(
      band_id, prob
    ) |>
    dplyr::summarise() ->
    iso_poly_st

  return(list(iso_poly_st))

}


#' Density Area
#' @export

density_area <- function(x, y, probs = 0.5, drop_geometry = T, ...){

  density_polygons(
    x = x,
    y = y,
    probs = probs,
    as_st = T,
    ...
  ) ->
    iso_poly_st

  iso_poly_st |>
    map(
      \(x) x |>
        dplyr::mutate(
          area = sf::st_area(geometry)
        )
    ) -> area_poly

  if(drop_geometry){
    area_poly |>
      map(
        \(x) x |> sf::st_drop_geometry()
      )->
      area_poly
  }

  return(area_poly)
}

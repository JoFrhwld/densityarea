expand_range <- function(x, mult = 0.25, ...){
  data_range <- range(x)
  range_diff <- diff(data_range)
  out_range <- ((range_diff * mult) * c(-1, 1)) + data_range
  return(out_range)
}

get_isolines<- function(x,
                         y,
                         probs = 0.5,
                         range_mult = 0.25,
                         ...) {
  rangex = expand_range(x, range_mult)
  rangey = expand_range(y, range_mult)

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

get_isolines_safely <- function(...){

  empty_iso <- tibble::tibble(line = NA_character_,
                      x = NA_real_,
                      y = NA_real_,
                      id = NA_integer_)

  purrr::safely(get_isolines,
                otherwise = empty_iso,
                quiet = TRUE)(...)->
    iso_result

  if(!is.null(iso_result$error)){
    dots <- rlang::dots_list(...)
    data_len <- length(dots$x)
    cli::cli_warn(
      c("There was a problem calculating probability isolines.",
        "i" = "There {?was/were} {data_len} x,y pair{?s} in the input.")
    )
  }

  return(iso_result$result)
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
#' @example inst/examples/density_polygon_example.R
#'
#' @importFrom dplyr .data
#' @import sf
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
  names(nameswap) <- vctrs::vec_as_names(c(xname, yname),
                                         repair = "unique",
                                         quiet = TRUE)

  isolines <- get_isolines_safely(x=x, y=y, probs=probs, ...)

  isolines |>
    dplyr::mutate(
      line_id = .data$line |>
        as.numeric() |>
        dplyr::desc() |>
        as.factor() |>
        as.numeric(),
      prob = sort(probs)[.data$line_id],
      order = dplyr::row_number()
    ) |>
    dplyr::select(-"line") |>
    dplyr::select("line_id",
           "id",
           "prob",
           "x",
           "y",
           "order") |>
    dplyr::rename(dplyr::any_of(nameswap)) ->
    iso_poly_df

  if (!as_sf & as_list) {
    return(list(iso_poly_df))
  } else if (!as_sf) {
    return(iso_poly_df)
  }

  if(nrow(iso_poly_df) < 4){
    iso_poly_st <- NULL
  }else{
    iso_poly_df |>
      dplyr::mutate(
        polygon_id = paste(.data$line_id, .data$id, sep = "-")
      ) |>
      sfheaders::sf_polygon(
        x = xname,
        y = yname,
        polygon_id = "polygon_id",
        keep = T
      ) |>
      dplyr::select(-"polygon_id") |>
      sf::st_sf() |>
      dplyr::group_by(
        .data$line_id, .data$prob
      ) |>
      dplyr::summarise() ->
      iso_poly_st
  }

  if (as_list) {
    return(list(iso_poly_st))
  } else{
    return(iso_poly_st)
  }
}


#' Density Area
#'
#' @description
#' A convenience function to get just the areas of density polygons.
#'
#'
#' @param x,y Numeric data dimensions
#' @param probs Probabilities to compute density polygons for
#' @param as_sf Should the returned values be [sf::sf]? Defaults to `FALSE`.
#' @param as_list Should the returned value be a list? Defaults to `TRUE` to
#' work well with tidyverse list columns
#' @param ... Additional arguments to be passed to [ggdensity::get_hdr()]
#'
#' @example inst/examples/density_area_example.R
#'
#' @importFrom dplyr .data
#'
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
    as_list = F,
    ...
  ) ->
    iso_poly_sf

  if(!is.null(iso_poly_sf)){
    iso_poly_sf |>
      sf::st_sf() |>
      dplyr::mutate(
        area = sf::st_area(.data$geometry)
      ) ->
      area_poly

    if (!as_sf) {
      area_poly |>
        sf::st_drop_geometry() ->
        area_poly
    }

  }else{
    area_poly <- NULL
  }

  if (as_list) {
    area_poly <- list(area_poly)
  }

  return(area_poly)

}

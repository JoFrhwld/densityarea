#' @import rlang
get_isolines<- function(x,
                         y,
                         probs = 0.5,
                         rangex,
                         rangey,
                         ...) {

  dots <- rlang::dots_list(...)
  hdr_args <- rlang::fn_fmls(ggdensity::get_hdr)
  use_args <- dots[names(dots) %in% names(hdr_args)]

  tibble::tibble(x = x,
                 y = y) |>
    ggdensity::get_hdr(probs = probs,
                       rangex = rangex,
                       rangey = rangey,
                       !!!use_args) |>
    rlang::inject() ->
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


sf_polygon_safely <- function(...){

  purrr::safely(sfheaders::sf_polygon,
                quiet = TRUE)(...)->
    poly_result

  if(!is.null(poly_result$error)){
    cli::cli_warn(
      c("There was a problem creating a polygon with {.fun sfheaders::sf_polygon}",
        "i" = "Try examining the results of {.fun densityarea::density_polygons} with {.arg as_sf} set to {.val {FALSE}}.")
    )
    return(tibble::tibble())
  }
  return(poly_result$result)
}

isolines_to_df <- function(isolines, probs, nameswap){
  isolines |>
    dplyr::mutate(
      level_id = .data$line |>
        as.numeric() |>
        dplyr::desc() |>
        as.factor() |>
        as.integer(),
      prob = sort(probs)[.data$level_id],
      order = dplyr::row_number()
    ) |>
    dplyr::select(-"line") |>
    dplyr::select("level_id",
                  "id",
                  "prob",
                  "x",
                  "y",
                  "order") |>
    dplyr::rename(dplyr::any_of(nameswap)) ->
    iso_poly_df

  return(iso_poly_df)

}

iso_df_to_sf <- function(iso_poly_df, xname, yname){

  if(nrow(iso_poly_df) < 4){
    return(tibble::tibble())
  }

  iso_poly_df |>
    dplyr::mutate(
      polygon_id = paste(.data$level_id, .data$id, sep = "-")
    ) -> df_prepared

  df_prepared |>
    sf_polygon_safely(
      x = xname,
      y = yname,
      polygon_id = "polygon_id",
      keep = T
    ) -> iso_poly_pieces

  if(is.null(iso_poly_pieces)){
    return(tibble::tibble())
  }

  iso_poly_pieces |>
    dplyr::select(-"polygon_id") |>
    sf::st_sf() ->
    iso_poly_pieces_sf

  iso_poly_pieces_sf |>
    dplyr::group_by(
      .data$level_id, .data$prob
    ) |>
    dplyr::summarise() ->
    iso_poly_sf

  return(iso_poly_sf)

}

#' Density polygons
#'
#' @description
#' Given numeric vectors `x` and `y`, `density_polygons()` will return
#' a data frame, or list of a data frames, of the polygon defining 2d kernel
#' densities.
#'
#' @details
#' When using `density_polygons()` together with [dplyr::summarise()], `as_list`
#' should be `TRUE`.
#'
#'
#' @param x,y Numeric data dimensions
#' @param probs Probabilities to compute density polygons for
#' @param as_sf Should the returned values be [sf::sf]? Defaults to `FALSE`.
#' @param as_list Should the returned value be a list? Defaults to `FALSE` to
#' work with [dplyr::reframe()]
#' @param range_mult A multiplier to the range of `x` and `y` across which the
#' probability density will be estimated.
#' @param rangex,rangey Custom ranges across `x` and `y` ranges across which the
#' probability density will be estimated.
#' @param ... Additional arguments to be passed to [ggdensity::get_hdr()]
#'
#' @returns A list of data frames, if `as_list=TRUE`, or just a data frame,
#' if `as_list=FALSE`.
#'
#' ## Data frame output
#'
#' If `as_sf=FALSE`, the data frame has the following columns:
#' \describe{
#'  \item{level_id}{An integer id for each probability level}
#'  \item{id}{An integer id for each sub-polygon within a probabilty level}
#'  \item{prob}{The probability level (originally passed to `probs`)}
#'  \item{x, y}{The values along the original `x` and `y` dimensions defining
#'  the density polygon. These will be renamed to the original input variable
#'  names.}
#'  \item{order}{The original plotting order of the polygon points, for
#'  convenience.}
#' }
#'
#' ## sf output
#' If `as_sf=TRUE`, the data frame has the following columns:
#' \describe{
#'  \item{level_id}{An integer id for each probability level}
#'  \item{prob}{The probability level (originally passed to `probs`)}
#'  \item{geometry}{A column of [sf::st_polygon()]s.}
#' }
#'
#' This output will need to be passed to [sf::st_sf()] to utilize many of the
#' features of [sf::sf].
#'
#' @details
#' If both `rangex` and `rangey` are defined, `range_mult` will be disregarded.
#' If only one or the other of `rangex` and `rangey` are defined, `range_mult`
#' will be used to produce the range of the undefined one.
#'
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
                             as_list = FALSE,
                             range_mult = 0.25,
                             rangex = NULL,
                             rangey = NULL,
                             ...) {

  dots <- rlang::dots_list(...)

  ### Capture variable names for name swap

  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y))

  nameswap <- c("x", "y")
  names(nameswap) <- vctrs::vec_as_names(c(xname, yname),
                                         repair = "unique",
                                         quiet = TRUE)

  ### process data

  processed_data <- process_data(x=x,
                                 xname = xname,
                                 y=y,
                                 yname = yname,
                                 probs)

  list2env(processed_data, envir = environment())

  ### process ranges

  processed_ranges <- process_ranges(x = x,
                                     y = y,
                                     rangex = rangex,
                                     rangey = rangey,
                                     range_mult = range_mult)

  list2env(processed_ranges, envir = environment())

  isolines <- get_isolines_safely(x=x,
                                  y=y,
                                  probs=probs,
                                  rangex = rangex,
                                  rangey = rangey,
                                  ...)
  iso_poly_df <- isolines_to_df(isolines, probs, nameswap)

  if (!as_sf & as_list) {
    return(list(iso_poly_df))
  } else if (!as_sf) {
    return(iso_poly_df)
  }

  iso_poly_sf <- iso_df_to_sf(iso_poly_df, xname, yname)

  if (as_list) {
    return(list(iso_poly_sf))
  } else{
    return(iso_poly_sf)
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
#' @param range_mult A multiplier to the range of `x` and `y` across which the
#' probability density will be estimated.
#' @param rangex,rangey Custom ranges across `x` and `y` ranges across which the
#' probability density will be estimated.
#' @param ... Additional arguments to be passed to [ggdensity::get_hdr()]
#'
#' @details
#' If both `rangex` and `rangey` are defined, `range_mult` will be disregarded.
#' If only one or the other of `rangex` and `rangey` are defined, `range_mult`
#' will be used to produce the range of the undefined one.
#'
#' @returns A list of data frames, if `as_list=TRUE`, or just a data frame,
#' if `as_list=FALSE`.
#'
#' ## Data frame output
#'
#' If `as_sf=FALSE`, the data frame has the following columns:
#' \describe{
#'  \item{level_id}{An integer id for each probability level}
#'  \item{prob}{The probability level (originally passed to `probs`)}
#'  \item{area}{The area of the HDR polygon}
#' }
#'
#' ## sf output
#' If `as_sf=TRUE`, the data frame has the following columns:
#' \describe{
#'  \item{level_id}{An integer id for each probability level}
#'  \item{prob}{The probability level (originally passed to `probs`)}
#'  \item{geometry}{The `sf::st_polygon()` of the HDR}
#'  \item{area}{The area of the HDR polygon}
#' }
#'
#' @example inst/examples/density_area_example.R
#'
#' @importFrom dplyr .data
#'
#' @export

density_area <- function(x,
                         y,
                         probs = 0.5,
                         as_sf = FALSE,
                         as_list = FALSE,
                         range_mult = 0.25,
                         rangex = NULL,
                         rangey = NULL,
                         ...) {
  density_polygons(
    x = x,
    y = y,
    probs = probs,
    as_sf = T,
    as_list = F,
    range_mult = range_mult,
    rangex = rangex,
    rangey = rangey,
    ...
  ) ->
    iso_poly_sf

  if(nrow(iso_poly_sf) > 0){
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
    area_poly <- tibble::tibble()
  }

  if (as_list) {
    area_poly <- list(area_poly)
  }

  return(area_poly)

}

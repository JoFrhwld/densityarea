# unexported functions from ggplot and ggdensity

isoband_z_matrix <- function(data) {
  # Convert vector of data to raster
  x_pos <- as.integer(factor(data$x, levels = sort(unique(data$x))))
  y_pos <- as.integer(factor(data$y, levels = sort(unique(data$y))))

  nrow <- max(y_pos)
  ncol <- max(x_pos)

  raster <- matrix(NA_real_, nrow = nrow, ncol = ncol)
  raster[cbind(y_pos, x_pos)] <- data$z

  raster
}

xyz_to_isolines <- function(data, breaks) {
  isoband::isolines(
    x = sort(unique(data$x)),
    y = sort(unique(data$y)),
    z = isoband_z_matrix(data),
    levels = breaks[-length(breaks)]
  )
}

na_filter <- function(...){

  dots <- rlang::dots_list(...)
  dots_name <- names(dots)

  na_vec <- purrr::map(dots, is.na)

  if(purrr::reduce(na_vec, any)){
    na_loc <- purrr::reduce(na_vec, `|`)
    new_values <- purrr::map(dots, \(x)x[!na_loc])
    output <- list(
      filtered = TRUE,
      values = new_values,
      total  = purrr::map(na_vec, sum)
    )
  }else{
    output <- list(
      filtered = FALSE,
      values = dots,
      total  = purrr::map(na_vec, sum)
    )
  }

  return(output)

}

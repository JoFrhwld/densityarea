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

check_dim_class <- function(x, varname){
  if(!is.numeric(x)){
    cli::cli_abort(
      c("All dimensions must be numeric",
        "i" = "{.var {varname}} has class {.cls {class(x)}}.")
    )
  }
}

check_dim_size <- function(x, y, xname, yname){
  if(length(x) != length(y)){
    xlen = length(x)
    ylen = length(y)
    cli::cli_abort(
      c("Data dimensions must have the same length",
        "i" = "{.var {xname}} has {xlen} value{?s}.",
        "i" = "{.var {yname}} has {ylen} value{?s}.")
    )
  }
}

check_probs <- function(probs) {
  if (!is.numeric(probs)) {
    cli::cli_abort(
      c("{.var probs} must be numeric",
        "i" = "{.var probs} has class {.cls {class(probs)}}")
    )
  }

  if (!all(is.finite(probs))) {
    non_finites <- unique(probs[which(!is.finite(probs))])
    cli::cli_abort(
      c("All {.var probs} must be finite.",
        "i" = "{.var probs} included values of {.val {non_finites}}")
    )
  }

  if (any(probs <= 0)) {
    n_less <- sum(probs <= 0)
    cli::cli_abort(
      c("All {.var probs} must be greater than 0.",
        "i" = "{.var probs} contained {n_less} value{?s} <= 0.")
    )
  }

  if (any(probs >= 1)) {
    n_greater <- sum(probs >= 1)
    cli::cli_abort(
      c("All {.var probs} must be less than 1.",
        "i" = "{.var probs} contained {n_greater} value{?s} >= 1")
    )
  }
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

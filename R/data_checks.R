# functions for checking and pre-processing input data

# check that an input variable is numeric
check_dim_class <- function(x, varname){
  if(!is.numeric(x)){
    cli::cli_abort(
      c("All dimensions must be numeric",
        "i" = "{.var {varname}} has class {.cls {class(x)}}.")
    )
  }
}

# check that x and y inputs have the same length
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

# check for minimum argument size
check_min_size <- function(x, varname, min_size = 1){
  if(length(x) < min_size){
    cli::cli_abort(
      c("{.var {varname}} must have at least {.val {min_size}} value{?s}.",
        "i" = "{.var {varname}} has {.val {length(x)}} value{?s}")
    )
  }
}

# check for maximum argument size
check_max_size <- function(x, varname, max_size = 1){
  if(length(x) > max_size){
    cli::cli_abort(
      c("{.var {varname}} must have no more than {.val {max_size}} value{?s}.",
        "i" = "{.var {varname}} has {.val {length(x)}} value{?s}")
    )
  }
}

# check finite

check_finite <- function(x, varname){
  if(!all(is.finite(x))){
    non_finites <- unique(x[which(!is.finite(x))])
    cli::cli_abort(
      c("All values of {.var {varname}} must be finite.",
        "i" = "{.var {varname}} included values of {.val {non_finites}}")
    )
  }
}

# check the probabilities
check_probs <- function(probs) {

  check_dim_class(probs, "probs")
  check_min_size(probs, "probs", min_size = 1)
  check_finite(probs, "probs")

  if (any(probs <= 0 | probs >= 1)) {
    n_less <- sum(probs <= 0)
    n_greater <- sum(probs >= 1)
    cli::cli_abort(
      c("All {.var probs} must be greater than 0 and less than 1.",
        "i" = "{.var probs} contained {n_less} value{?s} <= 0.",
        "i" = "{.var probs} contained {n_greater} value{?s} >= 1")
    )
  }

}


# drop na values from input dimensions
na_filter <- function(...){

  dots <- rlang::dots_list(...)
  dots_name <- names(dots)

  na_vec <- purrr::map(dots, purrr::negate(is.finite))

  output <- list(
    filtered = FALSE,
    values = dots,
    total  = purrr::map(na_vec, sum)
  )


  if(purrr::reduce(na_vec, any)){
    na_loc <- purrr::reduce(na_vec, `|`)
    new_values <- purrr::map(dots, \(x)x[!na_loc])
    output$filtered <- TRUE
    output$values <- new_values
  }

  return(output)

}


process_data <- function(x, xname, y, yname, probs){

  check_dim_class(x, xname)
  check_dim_class(y, yname)
  check_dim_size(x, y, xname, yname)
  check_probs(probs)

  na_filtered <- na_filter(x = x, y = y)

  if(na_filtered$filtered){
    x <- na_filtered$values$x
    y <- na_filtered$values$y

    x_total <- na_filtered$total$x
    y_total <- na_filtered$total$y
    cli::cli_warn(
      c("Missing and non-finite values dropped",
        "i" = "{x_total} missing or non-finite value{?s} in {xname}",
        "i" = "{y_total} missing or non-finite value{?s} in {yname}"
      )
    )
  }

  return(
    list(
      x = x,
      y = y,
      probs = probs,
      xname = xname,
      yname = yname
    )
  )

}

# process ranges

expand_range <- function(x, mult = 0.25){
  data_range <- range(x)
  range_diff <- diff(data_range)
  out_range <- ((range_diff * mult) * c(-1, 1)) + data_range
  return(out_range)
}

process_ranges <- function(x, y, rangex, rangey, range_mult){
  is_def <- c(!is.null(rangex), !is.null(rangey))
  mult_def <- !is.null(range_mult)

  if(xor(is_def[1], is_def[2])){
    range_names = c("rangex", "rangey")
    cli::cli_warn(
      c("{.var {range_names[is_def]}} was defined but {.var {range_names[!is_def]}} was not.")
    )
  }

  if(all(is_def) & mult_def){
    cli::cli_warn(
      c("{.var rangey}, {.var rangey}, and {.var range_mult} all defined.",
        "i" = "{.var range_mult} will be disregarded.")
    )
  }

  if(!all(is_def) & mult_def){
    check_finite(range_mult, "range_mult")
    check_max_size(range_mult, "range_mult", max_size = 1)
    check_min_size(range_mult, "range_mult", min_size = 1)
  }

  if(!is.null(rangex)){
    check_finite(rangex, "rangex")
    check_min_size(rangex,"rangex", min_size = 2)
    check_max_size(rangex, "rangex", max_size = 2)
  }

  if(!is.null(rangey)){
    check_finite(rangey, "rangey")
    check_min_size(rangey, "rangey", min_size = 2)
    check_max_size(rangey, "rangey", max_size = 2)
  }

  if(!any(c(is_def, mult_def))){
    cli::cli_abort(
      c("None of {.var rangex}, {.var rangey}, or {.var range_mult} defined.",
        "i" = "The range across dimensions for estimating the probability polygons must be defined.")
    )
  }

  output <- list()
  if(!is.null(rangex)){
    output$rangex <- rangex
  }else{
    output$rangex <- expand_range(x, mult = range_mult)
  }

  if(!is.null(rangey)){
    output$rangey <- rangey
  }else{
    output$rangey <- expand_range(y, mult = range_mult)
  }

  return(output)

}

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

# check for minumum argument size
check_min_size <- function(x, varname, min_size = 1){
  if(length(x) < min_size){
    cli::cli_abort(
      c("{.var {varname}} must have at least {.val {min_size}} value{?s}.",
        "i" = "{.var {varname}} has {.val {length(x)}} value{?s}")
    )
  }
}

# check the probabilities
check_probs <- function(probs) {

  check_dim_class(probs, "probs")
  check_min_size(probs, "probs", min_size = 1)

  if (!all(is.finite(probs))) {
    non_finites <- unique(probs[which(!is.finite(probs))])
    cli::cli_abort(
      c("All {.var probs} must be finite.",
        "i" = "{.var probs} included values of {.val {non_finites}}")
    )
  }

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

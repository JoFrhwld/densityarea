test_that("`density_polygons()` returns list by default", {
  set.seed(10)
  x <- rnorm(100)
  y <- rnorm(100)
  output <- densityarea::density_polygons(x = x, y = y)
  expect_type(output,"list")
  expect_s3_class(output, NA)
  expect_s3_class(output[[1]], "data.frame")
})


test_that("`density_polygon()` returns dataframe when asked", {
  set.seed(10)
  x <- rnorm(100)
  y <- rnorm(100)
  output <- densityarea::density_polygons(x = x, y = y, as_list = F)
  expect_s3_class(output,"data.frame")
})


test_that("`density_area()` returns list by default", {
  set.seed(10)
  x <- rnorm(100)
  y <- rnorm(100)
  output <-   densityarea::density_area(x = x, y = y)
  expect_type(output,"list")
  expect_s3_class(output, NA)
  expect_s3_class(output[[1]], "data.frame")
})


test_that("`density_area()` returns a dataframe when asked", {
  set.seed(10)
  x <- rnorm(100)
  y <- rnorm(100)
  output <-   densityarea::density_area(x = x, y = y, as_list = F)
  expect_s3_class(output, "data.frame")
})




test_that("`density_polygons` returns sf when asked", {
  set.seed(10)
  x <- rnorm(100)
  y <- rnorm(100)
  output <- densityarea::density_polygons(x = x, y = y, as_sf = T, as_list = F)
  expect_s3_class(
    output,
    "sf"
  )
})

test_that("`density_polygons` does not return sf by default", {
  set.seed(10)
  x <- rnorm(100)
  y <- rnorm(100)
  output <- densityarea::density_polygons(x = x, y = y, as_list = F)
  expect_false(
    "sf" %in% class(output)
  )
})


test_that("`density_area` does not return geometry by default", {
  set.seed(10)
  x <- rnorm(100)
  y <- rnorm(100)
  output <- densityarea::density_area(x = x, y = y, as_list = F)
  expect_false(
    "sf" %in% class(output)
  )
})

test_that("`density_area` does not returns geometry when asked", {
  set.seed(10)
  x <- rnorm(100)
  y <- rnorm(100)
  output <- densityarea::density_area(x = x, y = y, as_sf = T, as_list = F)
  expect_s3_class(
    output,
    "sf"
  )
})

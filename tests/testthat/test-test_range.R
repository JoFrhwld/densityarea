set.seed(10)
alpha = rnorm(100)
beta = rnorm(100)

test_that("warnings for redundant range settings", {
  expect_warning(
    density_polygons(alpha, beta, rangex = c(0, 1), rangey = c(0, 1), range_mult = 0.5)
  )
  expect_warning(
    density_area(alpha, beta, rangex = c(0, 1), rangey = c(0, 1), range_mult = 0.5)
  )
})

test_that("warnings for only one defined range", {
  expect_warning(
    density_polygons(alpha, beta, rangex = c(-2,2))
  )

  expect_warning(
    density_polygons(alpha, beta, rangey = c(-2,2))
  )

  expect_warning(
    density_area(alpha, beta, rangex = c(-2,2))
  )

  expect_warning(
    density_area(alpha, beta, rangey = c(-2,2))
  )

})

test_that("errors for invalid ranges", {
  expect_error(
    density_polygons(alpha, beta, rangex = 1, rangey = c(-2, 2), range_mult = NULL)
  )

})


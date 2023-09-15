test_that("multi-modal densities work", {
  set.seed(10)
  x = c(rnorm(50), rnorm(50, mean = 3))
  y = c(rnorm(50), rnorm(50, mean = 3))
  output = density_polygons(x, y, probs = c(0.1), as_sf = T, as_list = F)
  expect_s3_class(
    output$geometry,
    "sfc_MULTIPOLYGON"
  )
})

test_that("multi-modal densities work", {
  set.seed(10)
  x = rnorm(100)
  y = rnorm(100)
  probs = ppoints(10)
  output = density_polygons(x, y, probs = probs, as_sf = T, as_list = F)
  expect_equal(
    length(probs),
    nrow(output)
  )

})

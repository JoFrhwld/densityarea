test_that("columns are renamed", {
  set.seed(10)
  alpha <- rnorm(100)
  beta <- rnorm(100)

  polygon_df <- densityarea::density_polygons(x = alpha,
                                              y = beta,
                                              as_list = FALSE)

  expect_in(
    c("alpha","beta"),
    names(polygon_df)
  )

})

test_that("columns are renamed and repaired", {
  set.seed(10)
  alpha <- rnorm(100)

  polygon_df <- densityarea::density_polygons(x = alpha,
                                              y = alpha,
                                              as_list = FALSE)

  expect_in(
    c("alpha...1","alpha...2"),
    names(polygon_df)
  )

})

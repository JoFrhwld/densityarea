test_that("range expansion works as expected", {
  x <- 4:10
  y <- -4:4
  range_x <- range(x)
  expand_range_x <- expand_range(x)

  range_y <- range(y)
  expand_range_y <- expand_range(y)

  expect_lt(expand_range_x[1],
            range_x[1])

  expect_gt(expand_range_x[2],
            range_x[2])

  expect_lt(expand_range_y[1],
            range_y[1])

  expect_gt(expand_range_y[2],
            range_y[2])

})


test_that("range expansion has expected size", {

  range_size <- 6
  mult1 <- 0.5
  mult2 <- 0.25
  x_start <- 1
  x_end <- x_start + range_size
  x <- seq(x_start, x_end)

  expand_range_x1 <- expand_range(x, mult = mult1)
  expand_range_x2 <- expand_range(x, mult = mult2)

  expect_equal(
    expand_range_x1[1],
    x_start - (range_size * mult1)
  )

  expect_equal(
    expand_range_x1[2],
    x_end + (range_size * mult1)
  )

  expect_equal(
    expand_range_x2[1],
    x_start - (range_size * mult2)
  )

  expect_equal(
    expand_range_x2[2],
    x_end + (range_size * mult2)
  )

})

# Setup ----
usethis::git_vaccinate()

desc::desc_coerce_authors_at_r()

usethis::use_author(
  given = "Josef",
  family = "Fruehwald",
  email = "jofrhwld@gmail.com",
  role = c("aut","cre")
)

usethis::use_roxygen_md()

desc::desc_set(
  "Title",
  "Areas of Bivarate Density Distributions"
)


desc::desc_set(
  "Description",
  "With bivarate data, it is possible to calculate 2-dimensional kernel density
  estimates that return polygons at given levels of probablity. `densityarea`
  calculates the area of the these density estimates.",
  normalize = T
)

usethis::use_gpl3_license()

usethis::use_readme_md()

fs::file_delete(
  here::here("NAMESPACE")
)

roxygen2::roxygenize()

# Connecting to github ----

usethis::use_github()


# Dependencies ----



## imports ----
usethis::use_package("isoband")
usethis::use_package("tibble")
usethis::use_package("dplyr")
usethis::use_package("purrr")
usethis::use_package("ggdensity")
usethis::use_package("sf")
usethis::use_package("tidyr")
usethis::use_package("sfheaders")
usethis::use_package("vctrs")
usethis::use_package("cli")
usethis::use_package("rlang")

## suggests ----
usethis::use_package("readr", type = "Suggests")
usethis::use_package("forcats", type = "Suggests")
usethis::use_package("ggplot2", type = "Suggests")
usethis::use_package("ragg", type = "Suggests")


# Data ----
usethis::use_data_raw("s01")

# Documentation ----

usethis::use_vignette(name = "densityarea", title = "Using `{densityarea}`")
usethis::use_vignette(name = "sf-operations", title = "Using `{sf}` operations")


# Tests ----

usethis::use_testthat(3)
usethis::use_test("list_return")
usethis::use_test("sf_return")
usethis::use_test("multi_polygon")
usethis::use_test("expand_range")
usethis::use_test("test_rename")

usethis::use_test("test_range")


# Github ----
usethis::use_github_action(name = "check-standard")
usethis::use_pkgdown()
usethis::use_pkgdown_github_pages()


# Docs

usethis::use_logo("logo.svg")

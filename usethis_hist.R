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





## code to prepare `s01` dataset goes here

s01 <- readr::read_delim(
  here::here("data-raw", "s01.txt"),
  col_types = readr::cols_only(
    name = "c",
    age = "c",
    sex = "c",
    word = "c",
    vowel = "c",
    plt_vclass = "c",
    ipa_vclass = "c",
    F1 = "d",
    F2 = "d",
    dur = "d"
  )
) |>
  dplyr::select(
    name,
    age,
    sex,
    word,
    vowel,
    plt_vclass,
    ipa_vclass,
    F1,
    F2,
    dur
  )


usethis::use_data(s01, overwrite = TRUE)

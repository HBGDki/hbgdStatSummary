context("cpp matching")


library(readr)
if (FALSE) {
  #  make correct data
  cp <- hbgd::cpp
  set.seed(54321)
  cp$random_time_var <- base::sample(
    letters[1:10], nrow(cp), replace = TRUE,
    prob = c(991, rep(1, 9)) / 1000
  )
  cp$random_time_var[sample.int(nrow(cp), 8)] <- NA
  write_csv(cp, "tests/testthat/cp_data.csv")

  cp <- read_csv("tests/testthat/cp_data.csv", trim_ws = FALSE)
  summarize_dataset_json(cp, check = FALSE) %>%
    cat(file = "tests/testthat/cp_summary_valid.json")
  cat("\n", file = "tests/testthat/cp_summary_valid.json", append = TRUE)
}



test_that("data json matches valid data json", {

  valid_json <- readLines("cp_summary_valid.json") %>% paste(collapse = "\n")

  cp <- read_csv("cp_data.csv", trim_ws = FALSE)

  test_json <- summarize_dataset_json(cp, check = FALSE)

  expect_output_file(cat(test_json), "cp_summary_valid.json")
})

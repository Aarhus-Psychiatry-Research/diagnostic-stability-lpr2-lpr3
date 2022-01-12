library("pacman")

p_load(testthat, here)

source(here("src", "functions.r"))

test_dfs <- list()

test_that("Basic utility functions", {

  df_general <- data.frame(
    stringsAsFactors = FALSE,
    adiagnosekode = c("F203", "F203", "G8410", "F99"),
    prioritet = c("Planlagt", "Planlagt", "Planlagt", "Planlagt")
  )
  
  df_psych_only <- keep_only_psych_visits(df_general)
  
  expect_equal(df_psych_only$adiagnosekode, c("F203", "F203"))
  expect_equal(df_psych_only$prioritet, c("Planlagt", "Planlagt"))
  expect_equal(df_psych_only$prioritet, c("Planlagt", "Planlagt"))
  expect_equal(df_psych_only$prioritet, c("Planlagt", "Planlagt"))

})
source(here("src", "functions.r"))

df_general <- data.frame(
  adiagnosekode = c("F203", "F203", "G8410", "F99"),
  prioritet = c("Planlagt", "Planlagt", "Planlagt", "Planlagt")
)

df_psych_only <- data.frame(
  adiagnosekode = c("F203", "F203"),
  prioritet = c("Planlagt", "Planlagt")
)

test_that("Basic utility functions", {
  expect_equal(keep_only_psych_visits(df_general), df_psych_only, check.attributes = FALSE)
})
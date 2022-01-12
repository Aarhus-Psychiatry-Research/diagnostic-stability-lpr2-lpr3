library("pacman")

p_load(testthat, here, waldo)

source(here("src", "functions.r"))

test_file(here("tests", "testthat", "test-functions.r"))


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

})


test_that("convert_visits_to_sequences", {

  df_convert_seq_to_visits_in <- data.frame(
    stringsAsFactors = FALSE,
    constructed_id = c("1660002P1","1660002P1","1660002P2","1660002P2","1660002P3"),
              date = c("2016-01-14","2016-02-01","2016-02-29","2016-10-10","2017-01-27")
  ) 

  df_convert_seq_to_visits_out <- convert_visits_to_sequences(df_convert_seq_to_visits_in,
                                                              sequence_id_col = constructed_id,
                                                              date_col = date)

  expect_equal(df_convert_seq_to_visits_out$constructed_id, c("1660002P1", "1660002P2", "1660002P3"))
  expect_equal(df_convert_seq_to_visits_out$sequence_start_date, c("2016-01-14", "2016-02-29", "2017-01-27"))
  expect_equal(df_convert_seq_to_visits_out$sequence_end_date, c("2016-02-01", "2016-10-10", "2017-01-27"))

})


test_that("convert_visits_to_sequences", {

  df_count_in <- tibble::tribble(
  ~constructed_id,        ~date, ~sequence_start_date, ~sequence_end_date,
  "1660002P1", "2016-01-14",         "2016-01-14",       "2016-02-01",
  "1660002P2", "2016-02-29",         "2016-02-27",       "2016-10-10",
  "1660002P3", "2016-02-29",         "2016-02-27",       "2016-10-10",
  "1660002P4", "2016-01-14",         "2016-02-27",       "2017-03-27",
  "1660002P5", "2017-01-12",         "2017-01-12",       "2017-03-27"
  ) %>% mutate(sequence_start_date = ymd(sequence_start_date),
             sequence_end_date = ymd(sequence_end_date))

  df_c_o <- count_open_sequences_in_period(df_count_in)

  expect_equal(df_c_o$date, ymd(c("2016-01-14","2016-04-14","2016-07-14", "2016-10-14","2017-01-14")))
  expect_equal(df_c_o$count, c(1, 3, 3, 1, 2))

})

test_that("Collapse sequences if overlapping", {
  df_test_collapse_sequences <- tibble::tribble(
  ~dw_ek_borger, ~sequence_start_date, ~sequence_end_date,
  "1660002P1", "2016-01-14",         "2017-03-01",
  "1660002P1", "2016-01-26",         "2016-02-27",
  "1660002P1", "2016-02-10",         "2016-04-01",
  "1660002P1", "2018-02-10",         "2018-04-01",
  "1660002P4", "2016-01-14",         "2016-02-27",
  "1660002P5", "2017-01-12",         "2017-01-12") %>%
  mutate(sequence_start_date = ymd(sequence_start_date),
         sequence_end_date = ymd(sequence_end_date)) %>% 
  collapse_sequences_if_same_patient()


  expect_equal(df_test_collapse_sequences$dw_ek_borger, c("1660002P1", "1660002P1", "1660002P4", "1660002P5"))
  expect_equal(df_test_collapse_sequences$sequence_start_date, ymd(c("2016-01-14","2018-02-10","2016-01-14","2017-01-12")))
})


### Test recoding functions
test_dfs$recode <- tibble::tribble(
  ~dw_sk_lpr3forloebsansvar, ~dw_sk_kontakt, ~adiagnosekode, ~datotid_start,
  1,             -1,         "F0", "2013-07-23",
  1,             -1,         "F1", "2013-07-24",
  1,             -1,         "F9", "2013-07-25",
  2,             -1,         "F9", "2013-07-23",
  2,             -1,         "F4", "2013-07-24",
  3,             -1,         "F5", "2013-07-23",
  -1,             1,         "F9", "2013-07-23",
  -1,             1,         "F5", "2013-07-23",
  -1,             1,         "F9", "2013-07-23",
  -1,             1,         "F4", "2013-07-23"
)

test_dfs$recode_most_severe <-test_dfs$recode %>% 
  recode_with_most_severe_diagnosis_for_sequence(id_col = dw_sk_lpr3forloebsansvar) %>% 
  arrange(dw_sk_lpr3forloebsansvar, dw_sk_kontakt)


test_that("Recode with most severe", {
  ## Testing 'test_dfs$recode_most_severe'                                    ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing class
  expect_equal(
    class(test_dfs$recode_most_severe),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    test_dfs$recode_most_severe[["dw_sk_lpr3forloebsansvar"]],
    c(-1, -1, -1, -1, 1, 1, 1, 2, 2, 3),
    tolerance = 1e-4)
  expect_equal(
    test_dfs$recode_most_severe[["dw_sk_kontakt"]],
    c(1, 1, 1, 1, -1, -1, -1, -1, -1, -1),
    tolerance = 1e-4)
  expect_equal(
    test_dfs$recode_most_severe[["adiagnosekode"]],
    c("F9", "F5", "F9", "F4", "F0", "F0", "F0", "F4", "F4", "F5"),
    fixed = TRUE)
  expect_equal(
    test_dfs$recode_most_severe[["datotid_start"]],
    c("2013-07-23", "2013-07-23", "2013-07-23", "2013-07-23", "2013-07-23", 
      "2013-07-25", "2013-07-24", "2013-07-24", "2013-07-23", "2013-07-23"),
    fixed = TRUE)
  # Testing column names
  expect_equal(
    names(test_dfs$recode_most_severe),
    c("dw_sk_lpr3forloebsansvar", "dw_sk_kontakt", "adiagnosekode", 
      "datotid_start"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(test_dfs$recode_most_severe),
    c("numeric", "numeric", "character", "character"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(test_dfs$recode_most_severe),
    c("double", "double", "character", "character"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(test_dfs$recode_most_severe),
    c(10L, 4L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(test_dfs$recode_most_severe)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'test_dfs$recode_most_severe'                           ####
})

### 

source(here("src", "functions.r"))

test_dfs$recode_most_severe <- test_dfs$recode %>% 
  mutate(datotid_start = ymd(datotid_start)) %>% 
  recode_diagnoses_with_last_in_sequence(id_col = dw_sk_lpr3forloebsansvar) %>% 
  arrange(dw_sk_lpr3forloebsansvar, dw_sk_kontakt)


test_that("Recode with last visit's diagnosekode", {
  ## Testing 'test_dfs$recode_most_severe'                                    ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing class
  expect_equal(
    class(test_dfs$recode_most_severe),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    test_dfs$recode_most_severe[["dw_sk_lpr3forloebsansvar"]],
    c(-1, -1, -1, -1, 1, 1, 1, 2, 2, 3),
    tolerance = 1e-4)
  expect_equal(
    test_dfs$recode_most_severe[["dw_sk_kontakt"]],
    c(1, 1, 1, 1, -1, -1, -1, -1, -1, -1),
    tolerance = 1e-4)
  expect_equal(
    test_dfs$recode_most_severe[["adiagnosekode"]],
    c("F9", "F5", "F9", "F4", "F9", "F9", "F9", "F4", "F4", "F5"),
    fixed = TRUE)
  expect_equal(
    test_dfs$recode_most_severe[["datotid_start"]],
    structure(c(15909, 15909, 15909, 15909, 15911, 15910, 15909, 15910, 
                15909, 15909), class = "Date"))
  # Testing column names
  expect_equal(
    names(test_dfs$recode_most_severe),
    c("dw_sk_lpr3forloebsansvar", "dw_sk_kontakt", "adiagnosekode", 
      "datotid_start"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(test_dfs$recode_most_severe),
    c("numeric", "numeric", "character", "Date"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(test_dfs$recode_most_severe),
    c("double", "double", "character", "double"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(test_dfs$recode_most_severe),
    c(10L, 4L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(test_dfs$recode_most_severe)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'test_dfs$recode_most_severe'                           ####
})


### Test construct_sequences
test_dfs$construct_sequences <- tibble::tribble(
  ~clinic_id, ~patient_id, ~adiagnosekode, ~datotid_start,
  1,             1,         "F0", "2013-07-23 00:00:00",
  1,             1,         "F1", "2013-07-24 00:00:00",
  1,             1,         "F9", "2013-09-25 00:00:00",
  2,             1,         "F9", "2013-07-23 00:00:00",
  2,             1,         "F4", "2013-07-24 00:00:00",
  3,             2,         "F5", "2013-07-23 00:00:00"
) %>% mutate(datotid_start = ymd_hms(datotid_start))

test_dfs$constructed_sequences <- test_dfs$construct_sequences %>% 
  construct_sequences(
    clinic_id_col = clinic_id,
    patient_id_col = patient_id,
    datetime_col = datotid_start,
    datetime_col_name = "datotid_start",
    threshold_months = 1
  )

test_that("Testing construct_sequences", {
  ## Testing 'test_dfs$constructed_sequences'                                 ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing class
  expect_equal(
    class(test_dfs$constructed_sequences),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    test_dfs$constructed_sequences[["clinic_id"]],
    c(1, 1, 1, 2, 2, 3),
    tolerance = 1e-4)
  expect_equal(
    test_dfs$constructed_sequences[["patient_id"]],
    c(1, 1, 1, 1, 1, 2),
    tolerance = 1e-4)
  expect_equal(
    test_dfs$constructed_sequences[["adiagnosekode"]],
    c("F0", "F1", "F9", "F9", "F4", "F5"),
    fixed = TRUE)
  expect_equal(
    test_dfs$constructed_sequences[["datotid_start"]],
    structure(c(1374537600, 1374624000, 1380067200, 1374537600, 1374624000, 
                1374537600), tzone = "UTC", class = c("POSIXct", "POSIXt")))
  expect_equal(
    test_dfs$constructed_sequences[["threshold_date"]],
    structure(c(NA, 1377129600, 1377216000, 1382659200, 1377129600, 
                1377216000), tzone = "UTC", class = c("POSIXct", "POSIXt")))
  expect_equal(
    test_dfs$constructed_sequences[["constructed_id"]],
    c("111", "111", "112", "121", "121", "231"),
    fixed = TRUE)
  # Testing column names
  expect_equal(
    names(test_dfs$constructed_sequences),
    c("clinic_id", "patient_id", "adiagnosekode", "datotid_start", "threshold_date", 
      "constructed_id"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(test_dfs$constructed_sequences),
    c("numeric", "numeric", "character", "POSIXct", "POSIXct", "character"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(test_dfs$constructed_sequences),
    c("double", "double", "character", "double", "double", "character"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(test_dfs$constructed_sequences),
    c(6L, 6L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(test_dfs$constructed_sequences)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'test_dfs$constructed_sequences'                        ####
})
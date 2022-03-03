library("pacman")
p_load(qwraps2, tidyverse, lubridate)

start_date_str <- "2013-01-01"
end_date_str <- "2021-06-01"
transition_date_str <- "2019-02-03"
n_stat_tests <- 65

summarise_n_pt_in_period <- function(df) {
  df_out <- df %>%
    group_by(period) %>%
    summarise(unique_pt_in_period = n_distinct(dw_ek_borger))

  return(df_out)
}

summarise_n_by_subcp_by_quarter <- function(df) {
  df_out <- df %>%
    group_by(adiagnosekode, period) %>%
    summarise(
      n_with_diagnosis_in_period = n_distinct(dw_ek_borger),
      n_lcl = poisson.test(n_distinct(dw_ek_borger), conf.level = 0.95)$conf.int[1],
      n_ucl = poisson.test(n_distinct(dw_ek_borger), conf.level = 0.95)$conf.int[2]
    )

  return(df_out)
}

prop_patients_with_diagnosis_by_subcp <- function(df1, df2) {
  df_out <- df1 %>%
    left_join(df2) %>%
    rowwise() %>%
    mutate(
      prop = n_with_diagnosis_in_period / unique_pt_in_period,
      prop_lcl = prop.test(n_with_diagnosis_in_period, unique_pt_in_period, conf.level = 0.95)$conf.int[1],
      prop_ucl = prop.test(n_with_diagnosis_in_period, unique_pt_in_period, conf.level = 0.95)$conf.int[2]
    ) %>%
    unnest() %>%
    mutate(date_str = as.yearqtr(period, format = "%y Q%q"))

  return(df_out)
}

#' Takes a dataframe with visits as rows,
#' and overwrites their adiagnosekode with the most severe from
#' the connected series of visits.
#'
#' Ignores visits if id_column == ignore_id
#'
#' @param id_col The ID column for the LPR2 visits
#' @param ignore_id IDs that a visit has in the LPR2/3 col if it doesn't belong to that LPR version
relabel_diag_most_severe <- function(df, id_col, ignore_id = -1) {
  df_lpr2 <- df %>% # Collect all LPR2-visits for later merging as they're not affected by mitigation strategies
    filter({{ id_col }} == ignore_id)

  df_lpr3 <- df %>%
    filter({{ id_col }} != ignore_id) %>% # Drop all LPR2 visits
    mutate(mildness = as.numeric(substring(adiagnosekode, 2, 2))) %>%
    mutate(mildness = ifelse(mildness == 1, 10, mildness)) %>%
    mutate(severity = 10 - mildness) %>% ## Flip the order of the severities, making F0 the most severe (10), and F10 the least severe (0)
    group_by({{ id_col }}) %>%
    arrange(desc(severity), by_group = TRUE) %>%
    mutate(adiagnosekode = first(adiagnosekode)) %>%
    select(-severity, -mildness) %>%
    ungroup()

  return(bind_rows(
    df_lpr3,
    df_lpr2
  ))
}

keep_only_psych_visits <- function(df) {
  df <- df %>%
    filter(substring(adiagnosekode, 1, 1) == "F") %>%
    filter(substring(adiagnosekode, 1, 3) != "F99")

  return(df)
}

add_quarter_column <- function(df) {
  twelve_days_in_seconds <- 12 * 24 * 60 * 60 ## Add 12 days to make the first day of LPR2 on the edge of a quarter

  df_out <- df %>%
    mutate(period = round_date(datotid_start + twelve_days_in_seconds, "3 months"))

  return(df_out)
}


#' Takes a dataframe with visits as rows,
#' and overwrites their adiagnosekode with the last from
#' the connected series of visits.
#'
#' Ignores visits if id_column == ignore_id
#'
#' @param id_col The ID column for the visits
#' @param ignore_id
relabel_diag_with_last_in_sequence <- function(df, id_col, ignore_id = -1) {
  df_lpr2 <- df %>% # Collect all LPR2-visits for later merging as they're not affected by mitigation strategies
    filter({{ id_col }} == ignore_id)

  df_lpr3 <- df %>%
    filter({{ id_col }} != ignore_id) %>% # Drop LPR2 diagnoses
    group_by({{ id_col }}) %>%
    arrange(desc(datotid_start), .group_by = TRUE) %>%
    mutate(adiagnosekode = first(adiagnosekode)) %>%
    ungroup()

  return(bind_rows(df_lpr3, df_lpr2))
}

gen_unique_diagnoses_pr_patient <- function(df, confidence_intervals = TRUE, truncation_levels = TRUE) {
  ## Count number of unique diagnoses at different truncation intervals

  ## Handle main
  if (truncation_levels == TRUE) {
    df <- df %>%
      group_by(period, dw_ek_borger) %>%
      summarise(
        unique_diagnoses_1 = n_distinct(period, substr(adiagnosekode, 1, 2), dw_ek_borger),
        unique_diagnoses_2 = n_distinct(period, substr(adiagnosekode, 1, 3), dw_ek_borger),
        unique_diagnoses_3 = n_distinct(period, substr(adiagnosekode, 1, 4), dw_ek_borger),
        unique_diagnoses_4 = n_distinct(period, substr(adiagnosekode, 1, 5), dw_ek_borger)
      )
  } else {
    df <- df %>%
      group_by(period, dw_ek_borger) %>%
      summarise(unique_diagnoses_2 = n_distinct(period, substr(adiagnosekode, 1, 3), dw_ek_borger))
  }

  ## Handle confidence intervals
  if (confidence_intervals == TRUE) {
    if (truncation_levels == TRUE) {

      ## Generate confidence intervals
      df <- df %>%
        group_by(period) %>%
        summarise(
          mean.ci.1 = list(mean_ci(unique_diagnoses_1)),
          mean.ci.2 = list(mean_ci(unique_diagnoses_2)),
          mean.ci.3 = list(mean_ci(unique_diagnoses_3)),
          mean.ci.4 = list(mean_ci(unique_diagnoses_4)),
          n = n()
        )

      truncation_levels_list <- list(1, 2, 3, 4)
    } else {
      df <- df %>%
        group_by(period) %>%
        summarise(
          mean.ci.2 = list(mean_ci(unique_diagnoses_2)),
          n = n()
        )

      truncation_levels_list <- list(2)
    }

    ## Unnest confidence intervals
    for (i in truncation_levels_list) { ## Expand estimates
      df <- df %>%
        unnest_wider(paste0("mean.ci.", i)) %>%
        rename(
          !!str_c("mean_", i) := mean,
          !!str_c("lcl_", i) := lcl,
          !!str_c("ucl_", i) := ucl
        )
    }
  } else {
    df <- df %>%
      unnest_wider(paste0("mean.ci.", i)) %>%
      rename(!!str_c("mean_", i) := mean) %>%
      mutate(period = as.Date(period))
  }

  return(df)
}

log_time <- function() {
  print(format(Sys.time(), "%d/%m/%y %H:%M:%S"))
}

#' Takes a dataframe with visits as rows. Constructs unique keys for each sequence based on
#' clinic ID and patient ID.
#'
#' @param df
#' @param clinic_id_col
#' @param patient_id_col
#'
#'
#' @return df_with_constructed_sequences

add_patient_clinic_uid <- function(df,
                                       clinic_id_col,
                                       patient_id_col) {
  df <- df %>% mutate(patient_clinic_uid = paste0({{ patient_id_col }}, {{ clinic_id_col }}))

  return(df)
}

create_mitigation_df <- function(df_default, df_most_severe, df_last_visit_only) {
  df_out <- df_default %>%
    mutate(origin = "Unmitigated") %>%
    bind_rows(mutate(df_most_severe, origin = "Most severe")) %>%
    bind_rows(mutate(df_last_visit_only, origin = "Final visit")) %>%
    mutate(period = as.Date(period))

  return(df_out)
}

nudge_min_and_max_values <- function(x, label, nudge_constant = NULL, nudge_frac = NULL) {
  if (!is.null(nudge_constant) & !is.null(nudge_frac)) {
    stop("Set one of nudge_constant or nudge_frac")
  }

  has_content <- if_else(label == "", FALSE, TRUE)
  if (sum(has_content) <= 1) {
    return(x)
  }

  min_x <- min(x[has_content])
  max_x <- max(x[has_content])

  if (!is.null(nudge_frac)) {
    x[x == min_x] <- min_x * (1 - nudge_frac)
    x[x == max_x] <- max_x * (1 + nudge_frac)
  }
  if (!is.null(nudge_constant)) {
    x[x == min_x] <- min_x - nudge_constant
    x[x == max_x] <- max_x + nudge_constant
  }
  return(x)
}

visits_to_processed_treatment_courses <- function(df, uid_col = "visits_to_processed_treatment_courses") {
  df_out <- df %>%
    mutate(date = as_date(datotid_start)) %>%
    select("dw_ek_borger", "date", {{uid_col}}) %>%
    convert_visits_to_sequences(
      uid_col = {{uid_col}},
      date_col = date
    ) %>%
    mutate(sequence_end_date = sequence_end_date + 180) %>% ## Pad sequence_end_date with 3 months, to count as active if last visit is within three months
    collapse_overlapping_sequences_from_same_patient()
}

convert_visits_to_sequences <- function(df_in, uid_col, date_col) {
  df_out <- df_in %>%
    group_by({{ uid_col }}) %>%
    mutate(
      sequence_start_date = min({{ date_col }}),
      sequence_end_date = max({{ date_col }})
    ) %>%
    filter(row_number() == 1)
}

collapse_overlapping_sequences_from_same_patient <- function(df) {
  df_out <- df %>%
    group_by(dw_ek_borger) %>%
    arrange(sequence_start_date, by_group = TRUE) %>%
    mutate(indx = c(
      0,
      cumsum(as.numeric(lead(sequence_start_date)) >
      cummax(as.numeric(sequence_end_date)))[-n()]
    )) %>%
    group_by(dw_ek_borger, indx) %>%
    summarise(
      sequence_start_date = min(sequence_start_date),
      sequence_end_date = max(sequence_end_date)
    ) %>%
    arrange(dw_ek_borger) %>%
    select(-indx)
}

count_open_sequences_in_period <- function(df_in) {
  quarters <- seq(min(df_in$sequence_start_date), max(df_in$sequence_end_date), by = "quarter")

  df_out <- data.frame(
    stringsAsFactors = FALSE,
    period = quarters,
    open_sequences = sapply(quarters, function(x) sum(x >= df_in$sequence_start_date & x <= df_in$sequence_end_date))
  )
}

count_uniq_diags_per_pt_in_period <- function(df, n_chars) {
  df_out <- df %>%
    mutate(period = as.Date(period)) %>%
    group_by(period) %>%
    summarise("unique_diagnoses_{{n_chars}}" := n_distinct(substr(adiagnosekode, 1, n_chars), dw_ek_borger))
}

add_poisson_columns <- function(df, count_col, opportunities_col) {
  df_out <- df %>%
    rowwise() %>%
    mutate(
      "estimate" := poisson.test({{ count_col }}, {{ opportunities_col }})$estimate,
      "lcl" := poisson.test({{ count_col }}, {{ opportunities_col }})$conf.int[1],
      "ucl" := poisson.test({{ count_col }}, {{ opportunities_col }})$conf.int[2]
    )
}

calc_diag_per_treatment_course <- function(df1, df2) {
  df_out <- df1 %>%
    right_join(df2) %>% 
    add_poisson_columns(count_col = unique_diagnoses_3, opportunities_col = open_sequences)
}

library(dplyr, warn.conflicts = FALSE)
library(DBI)
library(lubridate)
options(lubridate.fasttime=TRUE)

#' Drop visits that are within a given window from the last visit of a patient.
#' Due to the type of problem, this has to iterate over each row, so it's quite intensive.
#' For development, recommend to work on only a subset of your data, e.g. using
#' df %>% slice_sample(prop=0.01) %>% drop_visits_within_break_window(months=3)
#' 
#' 
#' @param df Dataframe to do this on
#' @param months How far away from a previous visit does a visit have to be to not get dropped (num, months)
#' @param print_progress Whether to print how many patients have been processed so far, defaults to false
#' @param verbose Whether to print debugging information
#' @param id_column_name Column name (str) with unique patient IDs, defaults to "dw_ek_borger".
#' @param date_column_name Column name (str) for dates, defaults to datotid_start. Expects the column to be datatype POSIXct. 
#' 
#' @return A new dataframe without the visits within the window
drop_visits_within_break_window <- function(df, 
                                             months, 
                                             print_progress = FALSE,
                                             verbose = FALSE,
                                             id_column_name="dw_ek_borger",
                                             date_column_name="datotid_start") {
  
  print("Sorting visits from first to last, this might take a while (20s for 2M visits)")
  arranged_df <- df %>% arrange(!!as.symbol(id_column_name),
                                !!as.symbol(date_column_name))
  
  current_CPR <- 0
  patient_i <- 0
  last_selected_date <- 0
  indeces_to_drop <- c()
  month_in_seconds <- 30 * 24 * 60 * 60 # Calculate seconds in a month to add to POSIXct.
  
  arranged_df_id_column <- arranged_df[ , id_column_name]
  arranged_df_date_column <- arranged_df[ , date_column_name]

  total_patients <- length(unique(arranged_df_id_column))
  
  for (i in 1:nrow(df)) {
    if (arranged_df_id_column[i] != current_CPR) { # Handle switching to new person
      current_CPR = arranged_df_id_column[i]
      last_selected_date = arranged_df_date_column[i]
      
      if (patient_i %% 100 == 0 ) {
        percent_complete_rounded <- round(patient_i/total_patients*100, digits = 1)
        print(str_c(percent_complete_rounded, "%: Processing patient nr. ", patient_i))
      }
      
      patient_i <- patient_i + 1
      
      next()
    }
    
    if (arranged_df_id_column[i] == current_CPR) { # Handle comparison of current visit to previous selected date
      if (arranged_df_date_column[i] < last_selected_date + month_in_seconds * months) {
        if (verbose == TRUE) {
          print(str_c("\n\nDropping index: ", i, " because ", arranged_df_date_column[i], " is smaller than ", last_selected_date + month_in_seconds))
          print(str_c("Processing visit's date: ", arranged_df_date_column[i]))
          print(str_c("Last prediction visit's date: ", last_selected_date))
        }
        
        indeces_to_drop <- c(indeces_to_drop, i)
      } else {
        last_selected_date <- arranged_df_date_column[i]
      }
    }
  }
  
  return(arranged_df %>% slice(-indeces_to_drop))
}

#' Saves a combined boxplot and histogram png file to 
#' working directory/figures/filename
#' 
#' @param df
#' @param x_var
#' @param filename
#' 
#' @return A ggplot object
gen_stacked_box_and_histogram_png <- function(df, x_var) {
  
  gg <- ggplot(df, aes(x={{x_var}})) +
    scale_x_continuous()
  
  hist <- gg +
    geom_histogram() 
  
  box <- gg +
    geom_boxplot()
  
  combined <- hist + box + plot_layout(nrow = 2, height = c(2, 1))
  
  return(combined)
}

#' Adds a column containing the delta between two dates in years
#' @param df
#' @param start_date Expects POSIXct datatype
#' @param end_date Expects POSIXct datatype
#' @param column_name The new column name
#' 
#' @return A new dataframe of df + the column. Column is positive if end_date is after start_date.
add_column_years_between_dates <- function(df, start_date, end_date, column_name) {
  df_out <- df %>% 
    mutate({{column_name}} := interval({{start_date}}, {{end_date}}) / years(1))
  
  return(df_out)
}

#' Groups by grouping_vars and keeps only the record with the highest value on max_col
#' 
#' @param df
#' @param grouping_vars
#' 
#' @return A df with only the min records. If returns an empty df, beware that you min_col might be of type chr.
#' 
group_and_keep_record_with_min_value_on_col <- function(df, grouping_vars, min_col) {
  df_out <- df %>% 
    group_by({{grouping_vars}}) %>% 
    slice(which.min({{min_col}}))
  
  return(df_out)
}

#' Fix typical issues with the SQL import we get
#' - Convert dates from chr to POSIXct
#' - Remove leading Ds from adiagnosis
#' - Lowercase all column names
#' 
#' @param df (df) The dataframe to do it to
#' 
#' @return A cleaned dataframe
clean_sql_import <- function(df) {
  df <- df %>%
    rename_with(tolower)%>% 
    mutate(across(matches(".*diagnosekode.*"), ~ substr(.x, 2, 99))) %>%  # Remove leading Ds from e.g. DF20
    mutate(across(matches("datotid.+"), ~ ymd_hms(.x))) %>% # Set type of datetime columns to POSIXct
    mutate(across(matches("(.)+dato"), ~ ymd(.x))) # Set type of date-columns to date

  return(df)
}

#' Get all contents from SQL view
#' @param fct The fact name
#' 
#' @return A dataframe of the fct
#' 
get_fct <- function(fct) {
  conn <- DBI::dbConnect(
    odbc::odbc(),
    Driver = "SQL Server",
    Server = "BI-DPA-PROD",
    database = "USR_PS_Forsk",
    Trusted_Connection = "TRUE"
  )

  return(dbGetQuery(conn, paste0("SELECT * FROM [fct].", fct)))
}

log_time <- function() {
  print(format(Sys.time(), '%d/%m/%y %H:%M:%S'))
}

setup_chunk_observation_window <- function () {
  source("src/how_big_an_observation_window.r")
  log_time()
}
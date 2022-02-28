library("pacman")
p_load(qwraps2, tidyverse, lubridate)

add_column_unique_pt_in_period <- function(df) {
  df_out <- df %>%
    group_by(period) %>%
    summarise(unique_pt_in_period = n_distinct(dw_ek_borger))
  
  return(df_out)
}

add_column_n_with_diagnosis_in_period <- function(df) {
  df_out <- df %>%
    group_by(adiagnosekode, period) %>%
    summarise(
      n_with_diagnosis_in_period = n_distinct(dw_ek_borger),
      n_lcl = poisson.test(n_distinct(dw_ek_borger), conf.level = 0.95)$conf.int[1],
      n_ucl = poisson.test(n_distinct(dw_ek_borger), conf.level = 0.95)$conf.int[2]
    )
  
  return(df_out)
}

prop_of_patients_with_diagnosis_by_subcp <- function(df1, df2) {
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

  df_lpr2 <- df %>% 
    filter({{ id_col }} == ignore_id)

  df_lpr3 <- df %>% 
    filter({{ id_col }} != ignore_id) %>% 
    mutate(mildness = as.numeric(substring(adiagnosekode, 2, 2))) %>%
    mutate(mildness = ifelse(mildness == 1, 10, mildness)) %>%
    mutate(severity = 10 - mildness) %>%   ## Flip the order of the severities, making F0 the most severe (10), and F9 the least severe (1)
    group_by({{ id_col }}) %>%
    arrange(desc(severity), by_group = TRUE) %>% 
    mutate(adiagnosekode = adiagnosekode[1]) %>% 
    select(-severity, -mildness) %>% 
    ungroup
    
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

truncate_diagnosis_to_letter_and_digit <- function(df) {
  return(df %>% mutate(adiagnosekode = substring(adiagnosekode, 1, 2)))
}


add_LPR23_quarter_column <- function(df) {
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
recode_diagnoses_with_last_in_sequence <- function(df, id_col, ignore_id = -1) {
  df_lpr2 <- df %>% 
    filter({{ id_col }} == ignore_id)

  df_lpr3 <- df %>% 
    filter({{ id_col }} != ignore_id) %>%
      group_by({{ id_col }}) %>%
      arrange(desc(datotid_start), .group_by = TRUE) %>%
      mutate(adiagnosekode = adiagnosekode[1]) %>% 
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

add_patient_and_clinic_uid <- function(
                        df, 
                        clinic_id_col, 
                        patient_id_col) {

  df <- df %>% mutate(patient_clinic_id = paste0({{ patient_id_col }}, {{ clinic_id_col }}))
  
  return(df)
}

create_mitigation_df <- function(df_default, df_most_severe, df_last_visit_only) {
  df_out <- df_default %>%
    filter(period > ymd("2012-12-31")) %>%
    mutate(origin = "outpatient_all") %>%
    bind_rows(mutate(df_most_severe, origin = "most_severe")) %>%
    bind_rows(mutate(df_last_visit_only, origin = "final_visit")) %>%
    mutate(period = as.Date(period)) %>%
    mutate(origin = case_when(
      origin == "outpatient_all" ~ "Unmitigated",
      origin == "final_visit" ~ "Final visit",
      origin == "most_severe" ~ "Most severe"
    ))
  
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

convert_to_sequences_for_incident_per_active <- function(df) {
  df_out <- df %>% 
    mutate(date = as_date(datotid_start)) %>% 
    filter(date >= "2012-07-01") %>% 
    select(dw_ek_borger, date, patient_clinic_id) %>% 
    convert_visits_to_sequences(sequence_id_col = patient_clinic_id,
                                date_col = date) %>% 
    mutate(sequence_end_date = sequence_end_date + 180) %>%  ## Pad sequence_end_date with 3 months, to count as active if last visit is within three months
    collapse_sequences_if_same_patient()
}

convert_visits_to_sequences <- function(df_in, sequence_id_col, date_col) {
  df_out <- df_in %>% 
    group_by({{sequence_id_col}}) %>% 
    mutate(sequence_start_date = min({{date_col}}),
           sequence_end_date = max({{date_col}})) %>% 
    filter(row_number() == 1)
}

count_open_sequences_in_period <- function(df_in) {
  dates <- seq(min(df_in$sequence_start_date), max(df_in$sequence_end_date), by = "quarter")
  
  df_out <- data.frame(stringsAsFactors = FALSE,
                      date = dates,
                      open_sequences = sapply(dates, function(x) sum(x >= df_in$sequence_start_date & x <= df_in$sequence_end_date))) %>% 
            rename(period = date) %>% 
            as_tibble()
}

count_unique_diagnoses_in_period <- function(df, number_of_chars=3) {
  df_out <- df %>% 
    mutate(period = as.Date(period)) %>% 
    group_by(period) %>% 
    summarise("unique_diagnoses_{{number_of_chars}}" := n_distinct(substr(adiagnosekode, 1, number_of_chars), dw_ek_borger))
}

add_poisson_test_column <- function(df, count_col, opportunities_col) {
  df_out <- df %>% 
    rowwise() %>% 
    mutate("estimate" := poisson.test({{count_col}}, {{opportunities_col}})$estimate,
           "lcl" := poisson.test({{count_col}}, {{opportunities_col}})$conf.int[1],
           "ucl" := poisson.test({{count_col}}, {{opportunities_col}})$conf.int[2])
}

join_and_calculate_diagnoses_per_open_sequence <- function(df1, df2) {
  df_out <- df1 %>% 
    right_join(df2) %>%
    filter(period > "2013-01-01",
           period < "2021-04-02")
  
  df_out <- df_out %>% 
    rowwise() %>% 
    mutate(diag_per_active = poisson.test(unique_diagnoses_3, open_sequences)$estimate,
           lcl = poisson.test(unique_diagnoses_3, open_sequences)$conf.int[1],
           ucl = poisson.test(unique_diagnoses_3, open_sequences)$conf.int[2])
}

collapse_sequences_if_same_patient <- function(df) {
  df_out <- df %>% 
    group_by(dw_ek_borger) %>% 
    arrange(sequence_start_date, by_group = TRUE) %>% 
    mutate(indx = c(0, 
      cumsum(as.numeric(lead(sequence_start_date)) >
      cummax(as.numeric(sequence_end_date)))[-n()])
    ) %>%
    group_by(dw_ek_borger, indx) %>%
    summarise(sequence_start_date = min(sequence_start_date), 
              sequence_end_date = max(sequence_end_date)) %>%
    arrange(dw_ek_borger) %>% 
    select(-indx)
}



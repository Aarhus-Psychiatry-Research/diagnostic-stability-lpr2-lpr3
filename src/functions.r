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

calc_prop_of_unique_patients_with_diagnosis <- function(df1, df2) {
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

#' Takes a dataframe with lpr2 and lpr3 outpatient visits as rows,
#' and overwrites their adiagnosekode with the most severe from
#' the connected series of visits
#' @param id_column_1 The ID column for the LPR2 visits
#' @param id_column_2 The ID column for the LPR3 visits
#' @param two_columns Whether to process one or two columns.
#' #TODO Refactor to take a list of columns and iterate over them.
recode_with_most_severe_diagnosis_for_sequence <- function(df, id_column_1, id_column_2 = NA, two_columns = FALSE) {
  df_severity <- df %>%
    mutate(mildness = as.numeric(substring(adiagnosekode, 2, 2))) %>%
    mutate(mildness = ifelse(mildness == 1, 10, mildness)) %>%
    mutate(severity = 10 - mildness) # Flip the order of the severities, making F0 the most severe (10), and F9 the least severe (1)

  if (two_columns == FALSE) {
    df_lpr2 <- df_severity %>%
      filter({{ id_column_1 }} != -1) %>%
      group_by({{ id_column_1 }}) %>%
      arrange(desc(severity), .by_group = TRUE) %>%
      mutate(adiagnosekode = adiagnosekode[1]) # Get the most severe diagnosekode and recode them all as that

    return(df_lpr2)
  } else {
    df_lpr3 <- df_severity %>%
      filter({{ id_column_2 }} != -1) %>%
      group_by({{ id_column_2 }}) %>%
      arrange(desc(severity), .by_group = TRUE) %>%
      mutate(adiagnosekode = adiagnosekode[1]) # Get the most severe diagnosekode and recode them all as that

    df_lpr2 <- df_severity %>%
      filter({{ id_column_1 }} != -1) %>%
      group_by({{ id_column_1 }}) %>%
      arrange(desc(severity), .by_group = TRUE) %>%
      mutate(adiagnosekode = adiagnosekode[1]) # Get the most severe diagnosekode and recode them all as that

    return(bind_rows(
      df_lpr3,
      df_lpr2
    ))
  }
}

keep_only_psych_visits <- function(df) {
  return(df %>% filter(substring(adiagnosekode, 1, 1) == "F"))
}

truncate_diagnosis_to_letter_and_digit <- function(df) {
  return(df %>% mutate(adiagnosekode = substring(adiagnosekode, 1, 2)))
}


add_LPR23_quarter_column <- function(df) {
  twelve_days_in_seconds <- 12 * 24 * 60 * 60 # Add 12 days to make the first day of LPR2 on the edge of a quarter

  df_out <- df %>%
    mutate(period = round_date(datotid_start + twelve_days_in_seconds, "3 months"))

  return(df_out)
}

recode_diagnoses_with_last_in_sequence <- function(df, id_column_1, id_column_2, two_columns = FALSE) {
  recode_with_last <- function(df, id_col) {
    df_out <- df %>%
      filter({{ id_col }} != -1) %>%
      group_by({{ id_col }}) %>%
      arrange(desc(datotid_start), .group_by = TRUE) %>%
      mutate(adiagnosekode = adiagnosekode[1])

    return(df_out)
  }

  if (two_columns == FALSE) {
    return(recode_with_last(df, {{ id_column_1 }}))
  } else {
    df_lpr3 <- df %>%
      recode_with_last({{ id_column_2 }})

    df_lpr2 <- df %>%
      recode_with_last({{ id_column_1 }})

    return(bind_rows(df_lpr3, df_lpr2))
  }
}

gen_unique_diagnoses_pr_patient <- function(df, confidence_intervals = TRUE, truncation_levels = TRUE) {
  # Count number of unique diagnoses at different truncation intervals

  # Handle main
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
      summarise(unique_diagnoses_1 = n_distinct(period, substr(adiagnosekode, 1, 3), dw_ek_borger))
  }

  # Handle confidence intervals
  if (confidence_intervals == TRUE) {
    if (truncation_levels == TRUE) {
      # Generate confidence intervals
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
          mean.ci.1 = list(mean_ci(unique_diagnoses_1)),
          n = n()
        )

      truncation_levels_list <- list(1)
    }

    # Unnest confidence intervals
    for (i in truncation_levels_list) { # Expand estimates
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
#' clinic ID and patient ID. If distance between visits is higher than threshold, create a new
#' unique clinic ID + patient ID key.
#'
#' Terminology:
#'  no_threshold_sequence: A connected series of outpatient visits, where their IDs are location + patient ID
#'
#' @param df
#' @param clinic_id_col
#' @param patient_id_col
#' @param time_threshold
#'
#' @return df_with_constructed_sequences

construct_sequences <- function(df, clinic_id_col, patient_id_col, date_col, threshold_months = 0, verbose = TRUE) {
  df <- df %>% mutate(no_threshold_constructed_id = paste0({{ patient_id_col }}, {{ clinic_id_col }}))

  if (threshold_months == 0) {
    df <- df %>%
      rename(constructed_id = no_threshold_constructed_id)
  } else {
    MONTH_IN_SECONDS <- 30 * 24 * 60 * 60 # Calculate seconds in a month to add to POSIXct.
    THRESHOLD_MONTHS_IN_SECONDS <- MONTH_IN_SECONDS * threshold_months

    df <- df %>%
      mutate(last_datotid_start = lag(datotid_start)) %>%
      mutate(threshold_date = last_datotid_start + THRESHOLD_MONTHS_IN_SECONDS) %>%
      mutate(split_sequence_vectorised = if_else(threshold_date < datotid_start, 1, 0)) %>%
      arrange(dw_ek_borger, no_threshold_constructed_id, {{ date_col }})

    current_no_threshold_id <- 0 # The ID constructed without taking thresholds into account
    current_threshold_sequence_id <- 0

    all_sequences_n <- 0

    DF_LENGTH <- nrow(df)

    prev_ids <- vector(length = DF_LENGTH)
    split_sequences <- vector(length = DF_LENGTH)
    constructed_ids <- vector(length = DF_LENGTH)

    total_sequences <- df %>%
      select(no_threshold_constructed_id) %>%
      n_distinct()

    constructed_ids <- c()

    for (i in 1:nrow(df)) {
      i_no_threshold_id <- df$no_threshold_constructed_id[i]

      # If no_threshold ID has changed, consider to be a new sequence
      if (i_no_threshold_id != current_no_threshold_id) {
        prev_ids[i] <- current_no_threshold_id

        current_threshold_id_sequence_nr <- 1

        current_no_threshold_id <- i_no_threshold_id
        current_threshold_sequence_id <- paste0(i_no_threshold_id, current_threshold_id_sequence_nr)

        if (all_sequences_n %% 1000 == 0) {
          percent_complete_rounded <- round(all_sequences_n / total_sequences * 100, digits = 1)
          print(str_c(percent_complete_rounded, "%: Processing sequence nr. ", all_sequences_n))
        }

        all_sequences_n <- all_sequences_n + 1

        constructed_ids[i] <- current_threshold_sequence_id

        next()
      }

      # Continue processing the same no_threshold sequence
      if (i_no_threshold_id == current_no_threshold_id) {
        prev_ids[i] <- current_no_threshold_id

        # Handle recoding if distance is larger than threshold
        if (isTRUE(df$split_sequence_vectorised[i] == 1)) {
          split_sequences[i] <- 1

          current_threshold_id_sequence_nr <- current_threshold_id_sequence_nr + 1
          current_threshold_sequence_id <- paste0(i_no_threshold_id, current_threshold_id_sequence_nr)
        }
      }
      constructed_ids[i] <- current_threshold_sequence_id
    }

    df$prev_id <- prev_ids
    df$split_sequence <- split_sequences
    df$constructed_id <- constructed_ids
  }

  return(df)
}

create_mitigation_df <- function(df_default, df_most_severe, df_last_visit_only) {
  df_out <- df_default %>%
    filter(period > ymd("2012-12-31")) %>%
    mutate(origin = "outpatient_all") %>%
    bind_rows(mutate(df_most_severe, origin = "most_severe")) %>%
    bind_rows(mutate(df_last_visit_only, origin = "final_visit")) %>%
    mutate(period = as.Date(period)) %>%
    filter(period > ymd("2019-01-01") | origin == "outpatient_all") %>%
    mutate(origin = case_when(
      origin == "outpatient_all" ~ "Unmitigated",
      origin == "final_visit" ~ "Final visit",
      origin == "most_severe" ~ "Most severe"
    ))

  return(df_out)
}
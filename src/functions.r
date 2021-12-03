add_column_unique_pt_in_period <- function(df) {
  df_out <- df %>% 
    group_by(period) %>% 
    summarise(unique_pt_in_period = n_distinct(dw_ek_borger))
  
  return(df_out)
}

add_column_n_with_diagnosis_in_period <- function(df) {
  df_out <- df %>% 
    group_by(adiagnosekode, period) %>% 
    summarise(n_with_diagnosis_in_period = n_distinct(dw_ek_borger),
              n_lcl = poisson.test(n_distinct(dw_ek_borger), conf.level = 0.95)$conf.int[1],
              n_ucl = poisson.test(n_distinct(dw_ek_borger), conf.level = 0.95)$conf.int[2])
  
  return(df_out)
}

calc_prop_of_unique_patients_with_diagnosis <- function(df1, df2) {
  df_out <- df1 %>% 
    left_join(df2) %>% 
      rowwise %>% 
      mutate(prop = n_with_diagnosis_in_period / unique_pt_in_period,
            prop_lcl = prop.test(n_with_diagnosis_in_period, unique_pt_in_period, conf.level = 0.95)$conf.int[1],
            prop_ucl = prop.test(n_with_diagnosis_in_period, unique_pt_in_period, conf.level = 0.95)$conf.int[2]) %>% 
      unnest %>% 
      mutate(date_str = as.yearqtr(period, format = "%y Q%q"))
  
  return(df_out)
}

#' Takes a dataframe with lpr2 and lpr3 outpatient visits as rows,
#' and overwrites their adiagnosekode with the most severe from 
#' the connected series of visits
#' @param lpr2_id_column The ID column for the LPR2 visits
#' @param lpr3_id_column The ID column for the LPR3 visits
#' @param two_columns Whether to process one or two columns. 
#' #TODO Refactor to take a list of columns and iterate over them.
recode_with_most_severe_diagnosis_for_sequence <- function(df, lpr2_id_column, lpr3_id_column=NA, two_columns = FALSE) {
  df_severity <- df %>% 
    mutate(mildness = as.numeric(substring(adiagnosekode, 2, 2))) %>% 
    mutate(mildness = ifelse(mildness == 1, 10, mildness)) %>% 
    mutate(severity = 10 - mildness)  # Flip the order of the severities, making F0 the most severe (10), and F9 the least severe (1)
  
  if (two_columns == FALSE) {
    df_lpr2 <- df_severity %>% 
      filter({{lpr2_id_column}} != -1) %>% 
      group_by({{lpr2_id_column}}) %>% 
      arrange(desc(severity), .by_group = TRUE) %>% 
      mutate(adiagnosekode = adiagnosekode[1]) # Get the most severe diagnosekode and recode them all as that

    return(df_lpr2)
  } else {
    df_lpr3 <- df_severity %>% 
      filter({{lpr3_id_column}} != -1) %>% 
      group_by({{lpr3_id_column}}) %>% 
      arrange(desc(severity), .by_group = TRUE) %>% 
      mutate(adiagnosekode = adiagnosekode[1]) # Get the most severe diagnosekode and recode them all as that
    
    df_lpr2 <- df_severity %>% 
      filter({{lpr2_id_column}} != -1) %>% 
      group_by({{lpr2_id_column}}) %>% 
      arrange(desc(severity), .by_group = TRUE) %>% 
      mutate(adiagnosekode = adiagnosekode[1]) # Get the most severe diagnosekode and recode them all as that

    return(bind_rows(df_lpr3,
                    df_lpr2))
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
    mutate(period=round_date(datotid_start + twelve_days_in_seconds, "3 months"))
    
  return(df_out)
}

recode_diagnoses_with_last_in_sequence <- function(df, lpr2_id_column, lpr3_id_column, two_columns = FALSE) {
  
  recode_with_last <- function(df, id_col) {
    df_out <- df %>% 
      filter({{id_col}} != -1) %>% 
      group_by({{id_col}}) %>% 
      arrange(desc(datotid_start), .group_by = TRUE) %>% 
      mutate(adiagnosekode = adiagnosekode[1])
    
    return(df_out)
  }

  if (two_columns == FALSE) {
    return(recode_with_last(df, {{lpr2_id_column}}))
  } else {
    df_lpr3 <- df %>% 
    recode_with_last({{lpr3_id_column}})
  
    df_lpr2 <- df %>% 
      recode_with_last({{lpr2_id_column}})
    
    return(bind_rows(df_lpr3, df_lpr2))
  }
}

gen_unique_diagnoses_pr_patient <- function(df, confidence_intervals=TRUE) {
    # Count number of unique diagnoses at different truncation intervals
    df <- df %>% 
    group_by(period, dw_ek_borger) %>% 
        summarise(unique_diagnoses_1 = n_distinct(period, substr(adiagnosekode, 1, 1), dw_ek_borger),
                    unique_diagnoses_2 = n_distinct(period, substr(adiagnosekode, 1, 2), dw_ek_borger),
                    unique_diagnoses_3 = n_distinct(period, substr(adiagnosekode, 1, 3), dw_ek_borger),
                    unique_diagnoses_4 = n_distinct(period, substr(adiagnosekode, 1, 4), dw_ek_borger))

    if (confidence_intervals == TRUE) {
        # Generate confidence intervals
        df <- df %>% 
            group_by(period) %>% 
            summarise(mean.ci.1 = list(mean_ci(unique_diagnoses_1)),
                        mean.ci.2 = list(mean_ci(unique_diagnoses_2)),
                        mean.ci.3 = list(mean_ci(unique_diagnoses_3)),
                        mean.ci.4 = list(mean_ci(unique_diagnoses_4)),
                        n = n())

        # Unnest confidence intervals
        for (i in list(1, 2, 3, 4)) { # Expand estimates
            df <- df %>% 
                unnest_wider(paste0("mean.ci.", i)) %>% 
                rename(!!str_c("mean_", i) := mean,
                        !!str_c("lcl_", i) := lcl, 
                        !!str_c("ucl_", i) := ucl)
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
  print(format(Sys.time(), '%d/%m/%y %H:%M:%S'))
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

construct_sequences <- function(df, clinic_id_col, patient_id_col, date_col, threshold_months=0, verbose = TRUE) {

  df <- df %>% mutate(no_threshold_constructed_id = paste0({{patient_id_col}}, {{clinic_id_col}}))

  if (threshold_months == 0) {
    df <- df %>% 
      rename(constructed_id = no_threshold_constructed_id)
  } else {
    df <- df %>% 
      mutate(constructed_id = 0) %>% 
      mutate(split_sequence = 0) %>% 
      mutate(prev_id = 0) %>% 
      arrange(no_threshold_constructed_id, {{date_col}})

    MONTH_IN_SECONDS <- 30 * 24 * 60 * 60 # Calculate seconds in a month to add to POSIXct.

    current_no_threshold_id <- 0 # The ID constructed without taking thresholds into account
    current_threshold_sequence_id <- 0
    
    patient_n <- 0
    last_date <- 0

    total_patients <- df %>% 
      select(dw_ek_borger) %>% 
      n_distinct()

    constructed_ids <- c()

    current_threshold_id_sequence_nr <- 1

    for (i in 1:nrow(df)) {
      i_date <- df$datotid_start[i]
      i_no_threshold_id <- df$no_threshold_constructed_id[i]

      # If no_threshold ID has changed, consider to be a new sequence
      if (i_no_threshold_id != current_no_threshold_id) {
        df$prev_id[i] <- current_no_threshold_id

        current_threshold_id_sequence_nr <- 1

        current_no_threshold_id <- i_no_threshold_id
        current_threshold_sequence_id <- paste0(i_no_threshold_id, current_threshold_id_sequence_nr)
        
        if (patient_n %% 100 == 0 ) {
          percent_complete_rounded <- round(patient_n/total_patients*100, digits = 1)
          print(str_c(percent_complete_rounded, "%: Processing patient nr. ", patient_n))
        }

        patient_n <- patient_n + 1

        df$constructed_id[i] <- current_threshold_sequence_id
        
        last_date <- i_date
        
        next()
      }

      # Continue processing the same no_threshold sequence
      if (i_no_threshold_id == current_no_threshold_id) { 
        df$prev_id[i] <- current_no_threshold_id

        # Handle recoding if distance is larger than threshold
        if (i_date > last_date + MONTH_IN_SECONDS * threshold_months) {
          df$split_sequence[i] <- 1

          current_threshold_id_sequence_nr <- current_threshold_id_sequence_nr + 1
          current_threshold_sequence_id <- paste0(i_no_threshold_id, current_threshold_id_sequence_nr)
        }
      }

      last_date <- i_date
      df$constructed_id[i] <- current_threshold_sequence_id
    }
  }

  return(df)
}
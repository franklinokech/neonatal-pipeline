library(dplyr)
library(tidyr)
library(openssl)
library(glue)

# ============================================
# transform_data()
# ============================================
transform_data <- function(pumwani_data, standard_data) {
  if (!is.data.frame(pumwani_data) || !is.data.frame(standard_data)) {
    stop("Both pumwani_data and standard_data must be data frames.")
  }
  
  log_message("Starting transform_data...", level = "DEBUG")
  log_message(glue("Pumwani columns: {paste(names(pumwani_data), collapse=', ')}"), level = "DEBUG")
  log_message(glue("Standard columns: {paste(names(standard_data), collapse=', ')}"), level = "DEBUG")
  
  # --------------------------------------------------------------------------
  # Step 1: Recode column values (using loops with numeric conversion)
  # --------------------------------------------------------------------------
  log_message("Applying recoding rules...", level = "DEBUG")
  
  recode_rules <- list(
    recode_3_to_neg1 = c(
      "referred_to_hospital", "born_where", "fever", "difficulty_breathing", 
      "diarrhoea", "severe_vomiting", "difficulty_feeding", "convulsions", 
      "partial_focal_fits", "apnoea", "high_pitched_cry", "maternal_hiv", 
      "maternal_arv_s", "baby_given_arv_s", "maternal_vdrl", 
      "glucose_test_results_units", "pen_route", "ceftr_route", 
      "amikacin_route", "feeding_route_prescribed", "glucose", "bilirubin", 
      "lp", "grunting","bulging_fontanelle","hbadmission","other_treatment","genta_route","crackles",
      "hb_hct","cefta_route","feeding_route_prescribed"
    ),
    recode_5_to_neg1 = c("cap_refill", "pallor_anaemia", "skin", "jaundice", "otcme", "genta_route"),
    recode_6_to_neg1 = c("pen_freq", "ceftr_freq","other_treatment_2","frequency","cefta_freq"),
    recode_2_to_zero = c("grunting", "oxygen_saturation_measured", "glucose",
                         "bilirubin", "lp","crackles","hb_hct","hbadmission"),
    recode_2_to_neg1 = c("amp_route"),
    recode_4_to_neg1 = c("time_to_start_feeds_after","gest_size","central_cyanosis","pen_dose_unit"),
    recode_0_to_2 = c("vital_signs_monitored_in_t"),
    recode_0_to_neg1 = c("number_of_times_pulse_rate","number_of_times_respirator","number_of_times_temp_monit",
                         "number_of_times_oxygen_sat"),
    recode_neg1_to_5 = c("baby_feeding_disch"),
    recode_4_to_3 = c("feeding_route_prescribed")
  )
  
  tryCatch({
    for (col in recode_rules$recode_3_to_neg1) {
      if (col %in% names(pumwani_data)) {
        col_num <- as.numeric(pumwani_data[[col]])
        pumwani_data[[col]] <- dplyr::recode(col_num, `3` = -1, .default = col_num)
      }
    }
    for (col in recode_rules$recode_5_to_neg1) {
      if (col %in% names(pumwani_data)) {
        col_num <- as.numeric(pumwani_data[[col]])
        pumwani_data[[col]] <- dplyr::recode(col_num, `5` = -1, .default = col_num)
      }
    }
    for (col in recode_rules$recode_6_to_neg1) {
      if (col %in% names(pumwani_data)) {
        col_num <- as.numeric(pumwani_data[[col]])
        pumwani_data[[col]] <- dplyr::recode(col_num, `6` = -1, .default = col_num)
      }
    }
    for (col in recode_rules$recode_2_to_zero) {
      if (col %in% names(pumwani_data)) {
        col_num <- as.numeric(pumwani_data[[col]])
        pumwani_data[[col]] <- dplyr::recode(col_num, `2` = 0, .default = col_num)
      }
    }
    for (col in recode_rules$recode_2_to_neg1) {
      if (col %in% names(pumwani_data)) {
        col_num <- as.numeric(pumwani_data[[col]])
        pumwani_data[[col]] <- dplyr::recode(col_num, `2` = -1, .default = col_num)
      }
    }
    for (col in recode_rules$recode_4_to_neg1) {
      if (col %in% names(pumwani_data)) {
        col_num <- as.numeric(pumwani_data[[col]])
        pumwani_data[[col]] <- dplyr::recode(col_num, `4` = -1, .default = col_num)
      }
    }
    for (col in recode_rules$recode_0_to_2) {
      if (col %in% names(pumwani_data)) {
        col_num <- as.numeric(pumwani_data[[col]])
        pumwani_data[[col]] <- dplyr::recode(col_num, `0` = 2, .default = col_num)
      }
    }
    for (col in recode_rules$recode_0_to_neg1) {
      if (col %in% names(pumwani_data)) {
        col_num <- as.numeric(pumwani_data[[col]])
        pumwani_data[[col]] <- dplyr::recode(col_num, `0` = -1, .default = col_num)
      }
    }
    for (col in recode_rules$recode_neg1_to_5) {
      if (col %in% names(pumwani_data)) {
        col_num <- as.numeric(pumwani_data[[col]])
        pumwani_data[[col]] <- dplyr::recode(col_num, `-1` = 5, .default = col_num)
      }
    }
    for (col in recode_rules$recode_4_to_3) {
      if (col %in% names(pumwani_data)) {
        col_num <- as.numeric(pumwani_data[[col]])
        pumwani_data[[col]] <- dplyr::recode(col_num, `4` = 3, .default = col_num)
      }
    }
    log_message("Recoding completed successfully.", level = "DEBUG")
  }, error = function(e) {
    log_message(glue("Recoding failed: {e$message}"), level = "ERROR")
    stop(e)
  })
  
  # --------------------------------------------------------------------------
  # Step 2: Rename columns (using a safe loop instead of dplyr::rename)
  # --------------------------------------------------------------------------
  log_message("Renaming columns...", level = "DEBUG")
  
  rename_rules <- c(
    'id' = 'survey_id',
    'random' = 'randomized',
    'ipno' = 'patients_ipno',
    'date_discharge' = 'date_of_discharge_death',
    'date_adm' = 'admission_date',
    't_seen' = 'time_baby_seen',
    'fever_duration_in_days' = 'fever_duration',
    'abnormalities____1' = 'abnormalities___7',
    'vital_signs_monitored___1' = 'vital_signs_monitored_f48___1',
    'vital_signs_monitored___2' = 'vital_signs_monitored_f48___2',
    'vital_signs_monitored___3' = 'vital_signs_monitored_f48___3',
    'vital_signs_monitored___4' = 'vital_signs_monitored_f48___4',
    'oxygen_sat_monitored' = 'oxygen_saturation_monitore',
    'no_of_times_oxy_monitored' = 'number_of_times_oxygen_sat',
    'referred_to' = 'refereed_where',
    'follow_up___1' = 'follow_up_care___1',
    'follow_up___2' = 'follow_up_care___2',
    'follow_up___3' = 'follow_up_care___3',
    'follow_up___4' = 'follow_up_care___4',
    'follow_up___5' = 'follow_up_care___5',
    'follow_up___6' = 'follow_up_care___6',
    'follow_up___7' = 'follow_up_care____1',
    'follow_up____1' = 'follow_up_care____1',
    'last_menstrual_period' = 'last_normal_menstrual_peri',
    'age_recorded' = 'agedoc',
    'age_days' = 'age',
    'child_sex' = 'gender',
    'birth_wt' = 'birth_weight',
    'eye_pus' = 'pus_from_the_eyes',
    'other_adm_diag_not_listed' = 'other_admission_diag_not_listed',
    'date_fluid_presc' = 'fldte',
    'fluid_feed_monitoring_chart' = 'fluid_feed_monitoring_char',
    'feed_fluid_monitorng_chart' = 'feed_fluid_monitoring_char',
    'intravenous_fluids_presc' = 'intravenous_fluids_prescri',
    'total_vol_of_other_fluid' = 'total_volume_of_other_flui',
    'other_feeds' = 'other_feed',
    'no_of_times_temp_monitored' = 'number_of_times_temp_monit',
    'no_of_times_resp_monitored' = 'number_of_times_respirator',
    'no_of_times_puls_monitored' = 'number_of_times_pulse_rate',
    'freq_of_administration' = 'frequency_of_administratio',
    'date_the_feeds_initiated' = 'date_the_feeds_are_initiat',
    'start_date_phototherapy' = 'date_of_phototherapy',
    'fluid_monitoring_chart' = 'fld_cht',
    'disch_death_summ' = 'dth_sum',
    'outcome' = 'otcme',
    'dsc_condition' = 'condition_on_discharge',
    'referred_where' = 'refereed_where',
    'dsc_dx1_primary' = 'ddgnsis',
    'other_discharge_diag' = 'any_other_disch_diag',
    'other_discharge_diag_1' = 'other_disch_diag_1',
    'other_discharge_diag_2' = 'other_disch_diag_2',
    'other_discharge_diag_3' = 'other_disch_diag_3',
    'other_discharge_diag_4' = 'other_disch_diag_4',
    'other_discharge_diag_5' = 'other_disch_diag_5',
    'any_other_disch_diag' = 'other_disch_diag_not_listed',
    'other_disch_diag_old' = 'disch_diag_not_listed',
    'other_discharge_diag_unlisted' = 'other_discharge_diagnosis'
  )
  
  tryCatch({
    # Loop over rename rules and rename safely
    for (new_name in names(rename_rules)) {
      old_name <- rename_rules[new_name]
      if (old_name %in% names(pumwani_data)) {
        names(pumwani_data)[names(pumwani_data) == old_name] <- new_name
        log_message(glue("Renamed '{old_name}' to '{new_name}'"), level = "DEBUG")
      } else {
        log_message(glue("Column '{old_name}' not found, skipping rename to '{new_name}'"), level = "WARN")
      }
    }
    log_message("Renaming completed successfully.", level = "DEBUG")
  }, error = function(e) {
    log_message(glue("Renaming failed: {e$message}"), level = "ERROR")
    stop(e)
  })
  
  # --------------------------------------------------------------------------
  # Step 3: Merge datasets
  # --------------------------------------------------------------------------
  log_message("Merging datasets...", level = "DEBUG")
  
  tryCatch({
    common_cols <- intersect(names(standard_data), names(pumwani_data))
    log_message(glue("Common columns: {paste(common_cols, collapse=', ')}"), level = "DEBUG")
    
    # Convert to character for common columns
    for (col in common_cols) {
      standard_data[[col]] <- as.character(standard_data[[col]])
      pumwani_data[[col]] <- as.character(pumwani_data[[col]])
    }
    
    # Add missing columns from standard to pumwani
    missing_columns <- setdiff(names(standard_data), names(pumwani_data))
    for (col in missing_columns) {
      pumwani_data[[col]] <- NA_character_
      log_message(glue("Added missing column: {col}"), level = "DEBUG")
    }
    
    combined_data <- dplyr::bind_rows(standard_data, pumwani_data)
    log_message(glue("Combined data has {nrow(combined_data)} rows."), level = "DEBUG")
  }, error = function(e) {
    log_message(glue("Merging failed: {e$message}"), level = "ERROR")
    stop(e)
  })
  
  # --------------------------------------------------------------------------
  # Step 4: De‑identify IPNO
  # --------------------------------------------------------------------------
  log_message("De-identifying IPNO...", level = "DEBUG")
  tryCatch({
    combined_data <- combined_data %>%
      dplyr::mutate(ipno = ifelse(is.na(ipno), NA, openssl::sha256(as.character(ipno))))
    log_message("IPNO de-identification completed.", level = "DEBUG")
  }, error = function(e) {
    log_message(glue("De-identification failed: {e$message}"), level = "ERROR")
    stop(e)
  })
  
  # --------------------------------------------------------------------------
  # Step 5: Recode hospital IDs
  # --------------------------------------------------------------------------
  log_message("Recoding hospital IDs...", level = "DEBUG")
  tryCatch({
    hosp_id_mappings <- c(
      "1" = "hosp1", "52" = "hosp2", "53" = "hosp3", "58" = "hosp4",
      "63" = "hosp5", "71" = "hosp6", "55" = "hosp7", "41" = "hosp8",
      "44" = "hosp9", "76" = "hosp10", "51" = "hosp11", "40" = "hosp12",
      "17" = "hosp13"
    )
    combined_data <- combined_data %>%
      dplyr::mutate(
        hosp_id = dplyr::recode(as.character(hosp_id), !!!hosp_id_mappings, .default = as.character(hosp_id))
      )
    log_message("Hospital ID recoding completed.", level = "DEBUG")
  }, error = function(e) {
    log_message(glue("Hospital ID recoding failed: {e$message}"), level = "ERROR")
    stop(e)
  })
  
  log_message("transform_data completed successfully.", level = "DEBUG")
  return(combined_data)
}

# ============================================
# get_redcap_variables()
# ============================================
get_redcap_variables <- function(redcap_uri, token) {
  if (!requireNamespace("REDCapR", quietly = TRUE)) {
    stop("The 'REDCapR' package is not installed. Install it using install.packages('REDCapR').")
  }
  
  metadata <- REDCapR::redcap_metadata_read(
    redcap_uri = redcap_uri,
    token = token
  )$data
  
  variables <- metadata %>%
    dplyr::filter(field_type != "checkbox") %>%
    dplyr::pull(field_name)
  
  return(variables)
}

# ============================================
# filter_data()
# ============================================
filter_data <- function(data, variables, hosp_ids) {
  if (!is.data.frame(data)) {
    stop("The input data must be a data frame.")
  }
  
  if (!all(variables %in% colnames(data))) {
    missing_vars <- setdiff(variables, colnames(data))
    stop(glue::glue("The following variables are missing in the data: {paste(missing_vars, collapse = ', ')}"))
  }
  
  if (!"hosp_id" %in% colnames(data)) {
    stop("The data does not contain the required 'hosp_id' column.")
  }
  
  filtered_data <- data %>%
    dplyr::filter(hosp_id %in% hosp_ids) %>%
    dplyr::select(dplyr::all_of(variables))
  
  return(filtered_data)
}
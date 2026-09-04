source("scripts/utils.R")
source("scripts/config.R")
source("scripts/email.R")
source("scripts/extract.R")
source("scripts/transform.R")
source("scripts/load.R")

main_pipeline <- function() {
  tryCatch({
    log_message("===== Pipeline started =====", level = "INFO")
    config <- load_config()
    
    # Debug: print session info
    log_message(glue("R version: {R.version.string}"), level = "DEBUG")
    log_message(glue("Installed packages: {paste(rownames(installed.packages()), collapse=', ')}"), level = "DEBUG")
    
    # Check if dplyr is available
    if (!requireNamespace("dplyr", quietly = TRUE)) {
      stop("dplyr package is not installed. Please install it with renv::install('dplyr')")
    } else {
      log_message("dplyr is installed.", level = "DEBUG")
      # Also check tidyr
      if (!requireNamespace("tidyr", quietly = TRUE)) {
        log_message("tidyr is not installed, but may not be needed.", level = "WARN")
      }
    }
    
    end_time <- Sys.time()
    start_time <- end_time - (config$window_days * 24 * 60 * 60)
    log_message(glue("Fetching records created/modified between {start_time} and {end_time}"), level = "INFO")
    
    cache_dir <- config$cache_dir
    standard_cache <- file.path(cache_dir, "standard_data.rds")
    pumwani_cache <- file.path(cache_dir, "pumwani_data.rds")
    
    standard_data <- fetch_redcap_data(
      config$standard_uri, config$standard_token,
      start_time, end_time,
      cache_file = standard_cache,
      use_cache = config$use_cache,
      refresh_cache = config$refresh_cache
    )
    pumwani_data <- fetch_redcap_data(
      config$pumwani_uri, config$pumwani_token,
      start_time, end_time,
      cache_file = pumwani_cache,
      use_cache = config$use_cache,
      refresh_cache = config$refresh_cache
    )
    
    log_message(glue("Standard data rows: {nrow(standard_data)}, Pumwani data rows: {nrow(pumwani_data)}"), level = "DEBUG")
    
    combined_data <- transform_data(pumwani_data, standard_data)
    log_message(glue("Combined data has {nrow(combined_data)} rows."), level = "INFO")
    
    nest_vars <- get_redcap_variables(config$nest_uri, config$nest_token)
    nest_vars_checkboxes <- c(
      "bilirubin_type___1", "bilirubin_type___2", "bilirubin_type____1",
      "urine___1", "urine___2", "urine____1",
      "chemistry_n___1", "chemistry_n___2", "chemistry_n___3",
      "chemistry_n___4", "chemistry_n___5", "chemistry_n___6",
      "chemistry_n____1",
      "vital_signs_monitored___1", "vital_signs_monitored___2",
      "vital_signs_monitored___3", "vital_signs_monitored___4",
      "vital_signs_monitored___5",
      "follow_up___1", "follow_up___2", "follow_up___3",
      "follow_up___4", "follow_up___5", "follow_up___6",
      "follow_up___7", "follow_up____1",
      "blood_culture_pos_orgns___1", "blood_culture_pos_orgns___2",
      "blood_culture_pos_orgns___3", "blood_culture_pos_orgns___4",
      "blood_culture_pos_orgns___5", "blood_culture_pos_orgns___6",
      "blood_culture_pos_orgns___7", "blood_culture_pos_orgns___8",
      "blood_culture_pos_orgns___9", "blood_culture_pos_orgns___10",
      "blood_culture_pos_orgns____1"
    )
    all_vars <- union(nest_vars, nest_vars_checkboxes)
    exclusion_vars <- c("time_baby_seen","date_feeds_only_presc",
                        "date_feeds_first_presc", "date_feeds_prescribed",
                        "eye_pus","oxygen_sat_monitored","dsc_condition",
                        "other_fluid_prescribed","infant_drugs","other_treatment_2",
                        "total_volume_of_iv_fluids","feed_volume")
    final_vars <- setdiff(all_vars, exclusion_vars)
    nest_hosp_ids <- paste0("hosp", 1:13)
    nest_data <- filter_data(combined_data, final_vars, nest_hosp_ids)
    log_message(glue("NEST data has {nrow(nest_data)} rows."), level = "INFO")
    
    push_to_redcap(nest_data, config$nest_uri, config$nest_token)
    
    send_email(
      subject = "NEST Pipeline SUCCESS",
      body = glue("Pipeline completed successfully.\n\nNumber of records pushed: {nrow(nest_data)}\nTime window: {start_time} to {end_time}"),
      from = config$email_user,
      to = config$email_to,
      smtp_host = Sys.getenv("EMAIL_SMTP_HOST", "smtp.gmail.com"),
      smtp_port = Sys.getenv("EMAIL_SMTP_PORT", "587"),
      user = config$email_user,
      pass = config$email_pass
    )
    
    log_message("===== Pipeline finished successfully =====", level = "INFO")
    
  }, error = function(e) {
    log_message(glue("Pipeline FAILED: {e$message}"), level = "ERROR")
    # Log the full stack trace if available
    log_message(glue("Call stack: {paste(sys.calls(), collapse='\n')}"), level = "DEBUG")
    config <- tryCatch(load_config(), error = function(e) NULL)
    if (!is.null(config)) {
      send_email(
        subject = "NEST Pipeline FAILED",
        body = glue("Error: {e$message}\n\nCheck logs for details."),
        from = config$email_user,
        to = config$email_to,
        smtp_host = Sys.getenv("EMAIL_SMTP_HOST", "smtp.gmail.com"),
        smtp_port = Sys.getenv("EMAIL_SMTP_PORT", "587"),
        user = config$email_user,
        pass = config$email_pass
      )
    }
    quit(status = 1)
  })
}

main_pipeline()
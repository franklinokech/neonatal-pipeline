library(REDCapR)
library(readr)

fetch_redcap_data <- function(redcap_uri, token,
                              datetime_range_begin = as.POSIXct(NA),
                              datetime_range_end = as.POSIXct(NA),
                              batch_size = 100L, interbatch_delay = 0.5,
                              cache_file = NULL, use_cache = FALSE, refresh_cache = FALSE) {
  
  if (use_cache && !is.null(cache_file) && file.exists(cache_file) && !refresh_cache) {
    log_message(glue("Loading cached data from {cache_file}"), level = "INFO")
    return(readRDS(cache_file))
  }
  
  log_message(glue("Fetching from {redcap_uri} with datetime range: {datetime_range_begin} to {datetime_range_end}"), level = "INFO")
  
  tryCatch({
    result <- redcap_read(
      redcap_uri = redcap_uri,
      token = token,
      datetime_range_begin = datetime_range_begin,
      datetime_range_end = datetime_range_end,
      batch_size = batch_size,
      interbatch_delay = interbatch_delay,
      raw_or_label = "raw",
      export_checkbox_label = FALSE,
      export_survey_fields = FALSE,
      export_data_access_groups = FALSE,
      col_types = cols(.default = "c"),
      verbose = FALSE
    )
    if (!result$success) {
      stop(glue("API call failed: {result$raw_text}"))
    }
    data <- result$data
    log_message(glue("Fetched {nrow(data)} records."), level = "INFO")
    
    if (use_cache && !is.null(cache_file)) {
      dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
      saveRDS(data, cache_file)
      log_message(glue("Cached data saved to {cache_file}"), level = "INFO")
    }
    
    data
  }, error = function(e) {
    log_message(glue("Error in fetch_redcap_data: {e$message}"), level = "ERROR")
    stop(e)
  })
}
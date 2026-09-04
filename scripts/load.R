# ============================================
# push_to_redcap()
# ============================================
push_to_redcap <- function(data, redcap_uri, token, batch_size = 100L, verbose = TRUE) {
  # Validate input
  if (missing(data) || missing(redcap_uri) || missing(token)) {
    log_message("You must provide the data, REDCap URI, and API token.")
  }
  
  # Ensure data is a data frame
  if (!is.data.frame(data)) {
    log_message("The input data must be a data frame.")
  }
  
  # Use REDCapR's redcap_write function
  tryCatch({
    result <- redcap_write(
      ds = data,
      redcap_uri = redcap_uri,
      token = token,
      batch_size = batch_size,
      verbose = verbose
    )
    
    # Check for success
    if (result$success) {
      log_message("Data successfully pushed to REDCap.")
    } else {
      log_message(glue("Failed to push data to REDCap. {result$raw_text}"))
    }
    
    return(result)
  }, error = function(e) {
    log_message(glue("An error occurred while pushing data to REDCap: {e$message}"))
    return(NULL)
  })
}
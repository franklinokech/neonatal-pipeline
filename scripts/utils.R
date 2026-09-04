library(lubridate)
library(glue)

log_message <- function(msg, level = "INFO", log_file = Sys.getenv("LOG_FILE", "pipeline.log")) {
  # Ensure the log directory exists
  log_dir <- dirname(log_file)
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- glue("[{timestamp}] {level}: {msg}")
  cat(line, "\n", file = log_file, append = TRUE)
  cat(line, "\n")   # also to console
}
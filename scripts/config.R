library(dotenv)
library(here)

load_config <- function() {
  dotenv::load_dot_env()
  
  required_vars <- c(
    "REDCAP_STANDARD_API_URL", "REDCAP_STANDARD_API_TOKEN",
    "REDCAP_PUMWANI_API_URL", "REDCAP_PUMWANI_API_TOKEN",
    "REDCAP_NEST_API_URL", "REDCAP_NEST_API_TOKEN",
    "EMAIL_USER", "EMAIL_PASS", "EMAIL_TO"
  )
  missing <- required_vars[!nzchar(Sys.getenv(required_vars))]
  if (length(missing) > 0) {
    stop(glue("Missing required environment variables: {paste(missing, collapse=', ')}"))
  }
  
  list(
    standard_uri = Sys.getenv("REDCAP_STANDARD_API_URL"),
    standard_token = Sys.getenv("REDCAP_STANDARD_API_TOKEN"),
    pumwani_uri = Sys.getenv("REDCAP_PUMWANI_API_URL"),
    pumwani_token = Sys.getenv("REDCAP_PUMWANI_API_TOKEN"),
    nest_uri = Sys.getenv("REDCAP_NEST_API_URL"),
    nest_token = Sys.getenv("REDCAP_NEST_API_TOKEN"),
    window_days = as.integer(Sys.getenv("INCREMENTAL_WINDOW_DAYS", "30")),
    email_user = Sys.getenv("EMAIL_USER"),
    email_pass = Sys.getenv("EMAIL_PASS"),
    email_to = Sys.getenv("EMAIL_TO"),
    log_file = Sys.getenv("LOG_FILE", "logs/pipeline.log"),
    use_cache = tolower(Sys.getenv("USE_CACHE", "false")) == "true",
    refresh_cache = tolower(Sys.getenv("REFRESH_CACHE", "false")) == "true",
    cache_dir = Sys.getenv("CACHE_DIR", "/app/cache")
  )
}
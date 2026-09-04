library(emayili)

send_email <- function(subject, body, from, to, smtp_host, smtp_port, user, pass) {
  tryCatch({
    # Create email with `text` (not `body`)
    email <- envelope(
      from = from,
      to = to,
      subject = subject,
      text = body
    )
    
    # Use Gmail shortcut – ignores smtp_host/port
    gmail_server <- gmail(username = user, password = pass)
    gmail_server(email)
    
    TRUE
  }, error = function(e) {
    log_message(glue("Failed to send email: {e$message}"), level = "ERROR")
    FALSE
  })
}
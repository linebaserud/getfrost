
err_messages <- function() {
  cat("-------------------------------------------------------\n")

  cat("HTTP Status Codes\n\n")

  cat("400\n")
  cat("Invalid parameter value or malformed request.\n\n")

  cat("401\n")
  cat("Unauthorized client ID.\n\n")

  cat("403\n")
  cat("Too many observations requested.\n\n")

  cat("404\n")
  cat("No data was found for the query parameters.\n\n")

  cat("412\n")
  cat("No available time series for the query parameters.\n\n")

  cat("429\n")
  cat("The service is busy. Too many requests in progress. Retry-After is set with the number of seconds before the request should be retried again.\n\n")

  cat("500\n")
  cat("Internal server error.\n\n")

  cat("503\n")
  cat("The service is busy. Too many requests in progress. Retry-After is set with the number of seconds before the request should be retried again.\n")

  cat("-------------------------------------------------------\n")
}

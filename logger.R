# A simple, reliable logging utility with no external dependencies

# Initialize the logger with a specific log file
# 
# @param log_file Path to the log file
# @param append Whether to append to an existing log file or create a new one
# @param log_to_console Whether to also log messages to the console
# @return Invisibly returns TRUE on success

if(!file.exists("./log")){dir.create("./log")}

initialize_logger <- function(log_file = "./log/application.log", 
                              append = TRUE, 
                              log_to_console = FALSE) {
  # Create environment to store logger configuration
  if (!exists("logger_env", envir = .GlobalEnv)) {
    assign("logger_env", new.env(), envir = .GlobalEnv)
  }
  
  # Store configuration in environment
  logger_env$log_file <- log_file
  logger_env$log_to_console <- log_to_console
  
  # Create or clear the log file if not appending
  if (!append) {
    tryCatch({
      file.create(log_file, showWarnings = FALSE)
    }, error = function(e) {
      warning(paste("Could not create log file:", e$message))
    })
  }
  
  # Write header to log file
  log_entry <- paste0(
    "==========================================================\n",
    "Log started at ", format(Sys.time(), "%d-%m-%Y %H:%M:%S"), "\n",
    "R version: ", R.version$version.string, "\n",
    "Platform: ", R.version$platform, "\n",
    "Working directory: ", getwd(), "\n",
    "==========================================================\n"
  )
  
  tryCatch({
    cat(log_entry, file = log_file, append = append)
  }, error = function(e) {
    warning(paste("Could not write to log file:", e$message))
  })
  
  return(invisible(TRUE))
}

# Internal function to write a log entry
# 
# @param level The log level (INFO, WARN, ERROR, DEBUG)
# @param message The message to log
# @return Invisibly returns TRUE on success

log_to_file <- function(level, message) {
  if (!exists("logger_env", envir = .GlobalEnv)) {
    # Initialize with defaults if not already initialized
    initialize_logger()
  }
  
  # Get configuration from environment
  log_file <- logger_env$log_file
  log_to_console <- logger_env$log_to_console
  
  # Format the log entry
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- sprintf("[%s] [%s] %s\n", timestamp, level, message)
  
  # Write to log file
  tryCatch({
    cat(log_entry, file = log_file, append = TRUE)
  }, error = function(e) {
    # If we can't write to the log file, output to console regardless of setting
    cat("Error writing to log file:", e$message, "\n")
    cat(log_entry)
    return(invisible(FALSE))
  })
  
  # Also log to console if configured
  if (log_to_console) {
    cat(log_entry)
  }
  
  return(invisible(TRUE))
}

# Log an informational message
# 
# @param message The message to log
# @return Invisibly returns TRUE on success

log_info <- function(message) {
  log_to_file("INFO", message)
}

# Log a warning message
# 
# @param message The message to log
# @return Invisibly returns TRUE on success

log_warn <- function(message) {
  log_to_file("WARN", message)
}

# Log an error message
# 
# @param message The message to log
# @return Invisibly returns TRUE on success

log_error <- function(message) {
  log_to_file("ERROR", message)
}

# Log a debug message
# 
# @param message The message to log
# @return Invisibly returns TRUE on success

log_debug <- function(message) {
  log_to_file("DEBUG", message)
}

#' Set whether to also log messages to the console
#' 
#' @param enabled TRUE to enable console logging, FALSE to disable
#' @return Invisibly returns the previous setting
set_console_logging <- function(enabled = TRUE) {
  if (!exists("logger_env", envir = .GlobalEnv)) {
    initialize_logger()
  }
  
  previous <- logger_env$log_to_console
  logger_env$log_to_console <- enabled
  return(invisible(previous))
}

# Change the log file
# 
# @param log_file Path to the new log file
# @param append Whether to append to an existing log file or create a new one
# @return Invisibly returns the previous log file path

set_log_file <- function(log_file, append = TRUE) {
  if (!exists("logger_env", envir = .GlobalEnv)) {
    initialize_logger()
  }
  
  previous <- logger_env$log_file
  logger_env$log_file <- log_file
  
  # Create or clear the log file if not appending
  if (!append) {
    tryCatch({
      file.create(log_file, showWarnings = FALSE)
    }, error = function(e) {
      warning(paste("Could not create log file:", e$message))
    })
  }
  
  return(invisible(previous))
}
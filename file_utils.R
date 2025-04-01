# Functions for handling file operations

source("logger.R")

# ==== Ensure a directory exists, create it if necessary ====
# 
# @param dir_path Directory path to check/create
# @return TRUE if directory exists or was created successfully, FALSE otherwise
# @export

ensure_directory <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    log_info(paste("Creating directory:", dir_path))
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  
  if (!dir.exists(dir_path)) {
    log_error(paste("Failed to create directory:", dir_path))
    return(FALSE)
  }
  
  return(TRUE)
}

# ==== Validate that a file exists ====
# 
# @param file_path Path to check
# @param stop_if_missing Whether to stop execution if file is missing
# @return TRUE if the file exists, FALSE or stops execution otherwise
# @export

validate_file_exists <- function(file_path, stop_if_missing = TRUE) {
  if (!file.exists(file_path)) {
    if (stop_if_missing) {
      log_error(paste("File does not exist:", file_path))
      stop(paste("File does not exist:", file_path))
    } else {
      log_warn(paste("File does not exist:", file_path))
      return(FALSE)
    }
  }
  return(TRUE)
}

# ==== Safely write content to a file ====
# 
# @param content Content to write
# @param file_path Path where file should be written
# @param append Whether to append to existing file or overwrite
# @return TRUE if successful, FALSE otherwise
# @export

safe_write_file <- function(content, file_path, append = FALSE) {
  tryCatch({
    # Ensure directory exists
    dir_path <- dirname(file_path)
    ensure_directory(dir_path)
    
    # Write the file
    write(content, file = file_path, append = append)
    log_info(paste("Successfully wrote file:", file_path))
    return(TRUE)
  }, error = function(e) {
    log_error(paste("Failed to write file:", file_path, "Error:", e$message))
    return(FALSE)
  })
}

# ==== Safely read a file ====
# 
# @param file_path Path to the file to read
# @param encoding Character encoding of the file
# @return File content as a character vector or NULL if reading failed
# @export

safe_read_file <- function(file_path, encoding = "UTF-8") {
  if (!validate_file_exists(file_path, stop_if_missing = FALSE)) {
    return(NULL)
  }
  
  tryCatch({
    content <- readLines(file_path, encoding = encoding)
    log_info(paste("Successfully read file:", file_path))
    return(content)
  }, error = function(e) {
    log_error(paste("Failed to read file:", file_path, "Error:", e$message))
    return(NULL)
  })
}

# ==== Get a temporary file path with a specific extension ====
# 
# @param prefix Prefix for temporary file name
# @param extension File extension (without the dot)
# @param dir Directory for temporary file
# @return A path to a temporary file
# @export

get_temp_file_path <- function(prefix = "temp", extension = "xml", dir = tempdir()) {
  ensure_directory(dir)
  file.path(dir, paste0(prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", extension))
}
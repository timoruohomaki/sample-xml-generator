# ===== FINNISH SSN =====

get_finnish_ssn_checksum <- function(ssn_string) {
  
  # Validate input format
  if (!grepl("^\\d{6}-\\d{3}$", ssn_string)) {
  
    # TODO: Use Logger
    stop("Input must be in format '000000-000'")
  }
  
  strdigits <- gsub("-", "", ssn_string)
  digits <- as.numeric(strdigits)
  modulo <- digits %% 31
  
  checksum_chars <- c(
    "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
    "A", "B", "C", "D", "E", "F", "H", "J", "K", "L",
    "M", "N", "P", "R", "S", "T", "U", "V", "W", "X", "Y"
  )

    return(checksum_chars[modulo + 1])
  
}

get_finnish_ssn <- function() {
  
  
  
}



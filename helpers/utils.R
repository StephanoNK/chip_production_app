# helpers/utils.R

# Helper function to read and convert data from the DB
read_and_convert_table <- function(conn, table_name, date_cols = c()) {
  df <- DBI::dbReadTable(conn, table_name)
  for (col in date_cols) {
    if (col %in% colnames(df)) {
      df[[col]] <- as.Date(df[[col]])
    }
  }
  return(df)
}

# --- Password Hashing and Verification Functions ---

# Hash a password using sodium
hash_password <- function(password) {
  sodium::password_store(password)
}

# Verify a password against a hash
verify_password <- function(hash, password) {
  # Ensure hash is a valid character string
  if (!is.character(hash) || nchar(hash) == 0) {
    return(FALSE)
  }
  tryCatch({
    sodium::password_verify(hash, password)
  }, error = function(e) {
    # If the hash is invalid or password is empty, return FALSE
    FALSE
  })
}

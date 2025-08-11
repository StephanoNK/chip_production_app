# scripts/migrate_passwords.R
#
# A one-time script to migrate plain-text passwords in the 'akun' table
# to a secure, hashed format using the sodium package.
#
# WARNING:
# 1. BACK UP YOUR DATABASE BEFORE RUNNING THIS SCRIPT.
# 2. This is a destructive action and should only be run ONCE.
#
# How to run:
# 1. Make sure your .Renviron file is set up with your DB credentials.
# 2. Open your R project.
# 3. Run `source("scripts/migrate_passwords.R")` in the R console.

# --- Load necessary libraries and files ---
suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(sodium)
})

# Source the database configuration and password hashing utilities
source("config/db_config.R")
source("helpers/utils.R")

# --- Main Migration Logic ---
migrate_passwords <- function() {
  
  conn <- NULL # Initialize conn to NULL
  
  # Establish a database connection
  tryCatch({
    conn <- get_db_conn()
    message("Successfully connected to the database.")
  }, error = function(e) {
    stop("Failed to connect to the database. Please check your .Renviron settings. Error: ", e$message)
  })
  
  on.exit({
    if (!is.null(conn) && DBI::dbIsValid(conn)) {
      DBI::dbDisconnect(conn)
      message("Database connection closed.")
    }
  })
  
  # Fetch all accounts
  message("Fetching user accounts...")
  accounts <- tryCatch({
    DBI::dbReadTable(conn, "akun")
  }, error = function(e) {
    stop("Failed to read 'akun' table. Error: ", e$message)
  })
  
  if (nrow(accounts) == 0) {
    message("No accounts found to migrate.")
    return(invisible())
  }
  
  message(paste("Found", nrow(accounts), "accounts to process."))
  
  # Loop through each account, hash the password, and update the database
  for (i in 1:nrow(accounts)) {
    
    account <- accounts[i, ]
    email <- account$email
    plain_password <- account$password
    
    # Check if the password already looks like a hash to avoid re-hashing
    if (startsWith(plain_password, "$argon2id$")) {
      message(paste("Skipping", email, "- password already appears to be hashed."))
      next
    }
    
    message(paste("Processing account:", email))
    
    # Hash the plain-text password
    hashed_password <- hash_password(plain_password)
    
    # Create a parameterized query to update the password securely
    update_query <- "UPDATE akun SET password = $1 WHERE email = $2"
    
    # Execute the update
    tryCatch({
      result <- DBI::dbExecute(conn, update_query, params = list(hashed_password, email))
      if (result == 1) {
        message(paste(" -> Successfully updated password for", email))
      } else {
        warning(paste(" -> Failed to update password for", email, "(0 rows affected)."))
      }
    }, error = function(e) {
      warning(paste(" -> An error occurred while updating", email, ":", e$message))
    })
  }
  
  message("\nPassword migration process complete!")
  
}

# --- Execute the migration ---
# Uncomment the line below to run the script.
migrate_passwords()

# We leave it commented out by default as a safety measure.
message("Password migration script loaded. To run the migration, uncomment the final line in this script and source the file again.")


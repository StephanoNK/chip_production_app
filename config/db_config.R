# config/db_config.R

# Securely load database credentials from environment variables
# Ensure these are set in your deployment environment (e.g., .Renviron file)
DB_HOST <- Sys.getenv("PG_HOST", "localhost")
DB_PORT <- as.numeric(Sys.getenv("PG_PORT", 5432))
DB_NAME <- Sys.getenv("PG_DBNAME", "chip_production_db")
DB_USER <- Sys.getenv("PG_USER", "shiny_user")
DB_PASSWORD <- Sys.getenv("PG_PASSWORD") # IMPORTANT: No default password

# Function to create a database connection
get_db_conn <- function() {
  # Check if the password is provided
  if (is.null(DB_PASSWORD) || DB_PASSWORD == "") {
    stop("Database password is not set. Please set the PG_PASSWORD environment variable.")
  }
  
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = DB_HOST,
    port = DB_PORT,
    dbname = DB_NAME,
    user = DB_USER,
    password = DB_PASSWORD
  )
}

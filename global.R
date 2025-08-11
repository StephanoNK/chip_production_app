# global.R

library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(rmarkdown)
library(tinytex)
library(shinyWidgets)
library(shinyjs)
library(lubridate)
library(DT)
library(DBI)
library(RPostgres)
library(qrcode)
library(sodium)
library(pool) # Added for database connection pooling
library(shinytoastr) # Added for better notifications

# --- LaTeX Check for PDF Reporting ---
# On startup, check if TinyTeX is installed. If not, rendering PDFs will fail.
# This message will appear in the R console when the app starts.
if (!tinytex::is_tinytex()) {
  message("TinyTeX is not installed. PDF report generation will fail.")
  message("Please run tinytex::install_tinytex() in the R console to install it.")
}


# Source configuration and utility files
source("config/db_config.R", local = TRUE)
source("helpers/utils.R", local = TRUE)

# Data Entry Modules
source("modules/data_entry_ui.R", local = TRUE)
source("modules/data_entry_server.R", local = TRUE)

# Admin Modules
source("modules/admin_barang_ui.R", local = TRUE)
source("modules/admin_barang_server.R", local = TRUE)
source("modules/admin_akun_ui.R", local = TRUE)
source("modules/admin_akun_server.R", local = TRUE)
source("modules/admin_analisis_ui.R", local = TRUE)
source("modules/admin_analisis_server.R", local = TRUE)
source("modules/admin_debug_ui.R", local = TRUE) 
source("modules/admin_debug_server.R", local = TRUE)

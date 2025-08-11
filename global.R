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
library(sodium) # Added for password hashing

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

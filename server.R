# server.R

server <- function(input, output, session) {
  # Create a database connection pool at the beginning of the Shiny session
  pool <- tryCatch({
    get_db_conn()
  }, error = function(e) {
    showModal(modalDialog(
      title = "Database Connection Error",
      paste("Could not connect to the database. Please check your configuration and ensure the database is running.", e$message),
      easyClose = TRUE,
      footer = NULL
    ))
    return(NULL)
  })
  
  # Stop the app if the connection failed
  if (is.null(pool)) {
    return()
  }
  
  # Ensure the connection pool is closed when the Shiny session ends
  onStop(function() {
    if (!is.null(pool)) {
      poolClose(pool)
      message("Database connection pool closed.")
    }
  })
  
  # --- FIX: Hide the logout button on initial app load ---
  shinyjs::hide("logout_btn")
  
  # Reactive value to store user role
  user_role <- reactiveVal(NULL)
  
  # Initialize reactive_data and load initial data from the DB using the pool
  reactive_data <- reactiveValues(
    akun = read_and_convert_table(pool, "akun"),
    assembler = read_and_convert_table(pool, "assembler", c("tanggal_start", "tanggal_stop")),
    tester = read_and_convert_table(pool, "tester", c("tanggal_testing")),
    packager = read_and_convert_table(pool, "packager", c("tanggal_packaging"))
  )
  
  # Reactive value to store the start time of the assembly process
  assembly_start_timestamp <- reactiveVal(NULL)
  # Reactive value to track if the assembly process is currently running
  is_assembly_running <- reactiveVal(FALSE)
  
  # Observe login button click
  observeEvent(input$login_btn, {
    email <- input$email
    password <- input$password
    
    req(reactive_data$akun)
    user_info <- reactive_data$akun %>%
      filter(email == !!email)
    
    if (nrow(user_info) == 1 && verify_password(user_info$password, password)) {
      user_role(user_info$role)
      shinyjs::hide("email")
      shinyjs::hide("password")
      shinyjs::hide("login_btn")
      shinyjs::show("logout_btn") # This correctly shows the button after login
      
      session$userData$email_logged_in <- email
      toastr_success(paste("Welcome", user_info$role, email), position = "top-right")
    } else {
      toastr_error("Invalid email or password. Please try again.", position = "top-center")
    }
  })
  
  # Observe logout button click
  observeEvent(input$logout_btn, {
    user_role(NULL)
    shinyjs::show("email")
    shinyjs::show("password")
    shinyjs::show("login_btn")
    shinyjs::hide("logout_btn") # This correctly hides the button on logout
    updateTextInput(session, "email", value = "")
    updateTextInput(session, "password", value = "")
    session$userData$email_logged_in <- NULL # Clear logged in email
    toastr_info("You have been successfully logged out.", position = "top-right")
  })
  
  # Render sidebar menu based on user role
  output$sidebar <- renderMenu({
    req(user_role())
    if (user_role() == "Admin") {
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Download Report", tabName = "download", icon = icon("download")),
        menuItem("Admin Dashboard", tabName = "admin_dashboard", icon = icon("user-secret"),
                 menuSubItem("Manage Product Data", tabName = "admin_barang"),
                 menuSubItem("Manage User Accounts", tabName = "admin_akun"),
                 menuSubItem("Data Analysis", tabName = "admin_analisis"),
                 menuSubItem("Debug Tools", tabName = "admin_debug", icon = icon("bug")) 
        )
      )
    } else {
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Data Entry", tabName = "entry", icon = icon("edit")),
        menuItem("Download Report", tabName = "download", icon = icon("download"))
      )
    }
  })
  
  # Render main UI content based on user role
  output$main_ui <- renderUI({
    req(user_role())
    
    data_entry_ns_func <- NS(session$ns("data_entry"))
    admin_barang_ns_func <- NS(session$ns("admin_barang"))
    admin_akun_ns_func <- NS(session$ns("admin_akun"))
    admin_analisis_ns_func <- NS(session$ns("admin_analisis"))
    admin_debug_ns_func <- NS(session$ns("admin_debug")) 
    
    tabItems(
      tabItem(tabName = "dashboard",
              h2("Chip Production Dashboard"),
              fluidRow(
                box(
                  title = "Filter Data", status = "primary", solidHeader = TRUE, width = 12,
                  dateRangeInput("filter_date", "Filter by Date",
                                 start = Sys.Date() - 30, end = Sys.Date(),
                                 format = "dd/mm/yyyy", separator = " to ")
                ),
                box(
                  title = "Production Volume per Stage", status = "info", solidHeader = TRUE, width = 12,
                  plotlyOutput("role_pie")
                )
              )
      ),
      data_entry_ui(user_role, data_entry_ns_func),
      tabItem(tabName = "download",
              h2("Download Production Report"),
              fluidRow(
                box(
                  title = "Download PDF Report", status = "info", solidHeader = TRUE, width = 6,
                  helpText("This report will contain a summary of production data based on the date filter in the Dashboard."),
                  downloadButton("download_report", "Download PDF Report", class = "btn-primary")
                )
              )
      ),
      admin_barang_ui(admin_barang_ns_func),
      admin_akun_ui(admin_akun_ns_func),
      admin_analisis_ui(admin_analisis_ns_func),
      admin_debug_ui(admin_debug_ns_func) 
    )
  })
  
  # Call server modules, passing the connection pool
  data_entry_server("data_entry", reactive_data, pool, assembly_start_timestamp, is_assembly_running)
  admin_barang_server("admin_barang", reactive_data, pool)
  admin_akun_server("admin_akun", reactive_data, pool)
  admin_analisis_server("admin_analisis", reactive_data)
  admin_debug_server("admin_debug", reactive_data, pool) 
  
  # --- Dashboard Plot ---
  output$role_pie <- renderPlotly({
    req(input$filter_date, reactive_data$assembler, reactive_data$tester, reactive_data$packager)
    
    assembler_filtered <- reactive_data$assembler %>%
      filter(tanggal_start >= input$filter_date[1] & tanggal_start <= input$filter_date[2])
    
    tester_filtered <- reactive_data$tester %>%
      filter(tanggal_testing >= input$filter_date[1] & tanggal_testing <= input$filter_date[2])
    
    packager_filtered <- reactive_data$packager %>%
      filter(tanggal_packaging >= input$filter_date[1] & tanggal_packaging <= input$filter_date[2])
    
    counts <- data.frame(
      Role = c("Assembler", "Tester", "Packager"),
      Count = c(nrow(assembler_filtered), nrow(tester_filtered), nrow(packager_filtered))
    )
    
    plot_ly(counts, labels = ~Role, values = ~Count, type = 'pie',
            textposition = 'inside', textinfo = 'percent+label',
            marker = list(colors = c('#1f77b4', '#ff7f0e', '#2ca02c'),
                          line = list(color = '#FFFFFF', width = 1))) %>%
      layout(title = 'Production Volume per Stage')
  })
  
  # --- Download Report ---
  output$download_report <- downloadHandler(
    filename = function() {
      paste("Chip_Production_Report_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("reports/report.Rmd", tempReport, overwrite = TRUE)
      
      req(reactive_data$assembler, reactive_data$tester, reactive_data$packager, input$filter_date)
      
      params <- list(
        assembler_data = reactive_data$assembler,
        tester_data = reactive_data$tester,
        packager_data = reactive_data$packager,
        filter_start_date = input$filter_date[1],
        filter_end_date = input$filter_date[2]
      )
      
      # Use tryCatch to handle potential rendering errors
      tryCatch({
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }, error = function(e) {
        # If rendering fails, show an error notification to the user
        toastr_error(
          title = "Report Generation Failed",
          message = "Could not generate the PDF report. Please check that LaTeX is installed and that the data filters are valid.",
          position = "top-center"
        )
        # Stop the app from trying to serve a non-existent or incorrect file
        stop(e)
      })
    }
  )
}
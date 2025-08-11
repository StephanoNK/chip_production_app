# ui.R

ui <- dashboardPage(
  dashboardHeader(title = "Aplikasi Produksi Chip"),
  dashboardSidebar(
    textInput("email", "Masukkan Email"),
    passwordInput("password", "Masukkan Password"),
    actionButton("login_btn", "Login", class = "btn-primary"),
    actionButton("logout_btn", "Logout", class = "btn-danger", style = "margin-top: 15px;"),
    sidebarMenuOutput("sidebar")
  ),
  dashboardBody(
    useShinyjs(),
    useToastr(), # Initialize shinytoastr
    # Add JsBarcode library
    tags$head(
      tags$script(src = "https://cdn.jsdelivr.net/npm/jsbarcode@latest/dist/JsBarcode.all.min.js")
    ),
    uiOutput("main_ui")
  )
)

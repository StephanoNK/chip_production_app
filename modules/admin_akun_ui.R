# modules/admin_akun_ui.R

admin_akun_ui <- function(ns) {
  tabItem(tabName = "admin_akun",
          h2("Admin: Manage User Accounts"),
          fluidRow(
            box(title = "Add New Account", status = "warning", solidHeader = TRUE, width = 12,
                textInput(ns("admin_akun_email"), "Email (must be unique)"),
                passwordInput(ns("admin_akun_password"), "Password"),
                selectInput(ns("admin_akun_role"), "Role", choices = c("Assembler", "Tester", "Packager", "Admin")),
                actionButton(ns("admin_add_akun"), "Add Account", class = "btn-success")
            ),
            box(title = "Account Data (Editable)", status = "primary", solidHeader = TRUE, width = 12,
                helpText("Double-click a cell to edit Role. Press Enter to save. Passwords are not displayed."),
                DT::dataTableOutput(ns("admin_akun_crud_table")),
                actionButton(ns("admin_change_password"), "Change Selected User's Password", class = "btn-warning", style = "margin-top: 10px;"),
                actionButton(ns("admin_delete_akun"), "Delete Selected Account", class = "btn-danger", style = "margin-top: 10px;")
            )
          )
  )
}

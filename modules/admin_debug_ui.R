# modules/admin_debug_ui.R

admin_debug_ui <- function(ns) {
  tabItem(tabName = "admin_debug",
          h2("Admin: Debug & Data Generation Tools"),
          fluidRow(
            box(title = "Generate Sample Data", status = "danger", solidHeader = TRUE, width = 12,
                p("Use these buttons to populate the database with random sample data for testing purposes. This can be useful for development and demonstrations."),
                p(strong("Warning:"), "This will add a significant amount of data to the database and actions cannot be undone easily."),
                hr(),
                actionButton(ns("debug_create_units"), "Create 100 Assembler Units", class = "btn-primary"),
                actionButton(ns("debug_test_units"), "Test 100 Units", class = "btn-primary"),
                actionButton(ns("debug_pack_units"), "Package Passed Units", class = "btn-primary"),
                hr(),
                p("This action will permanently delete all data from the assembler, tester, and packager tables. Use with extreme caution."),
                actionButton(ns("debug_clear_all_data"), "Clear All Production Data", class = "btn-danger")
            )
          )
  )
}

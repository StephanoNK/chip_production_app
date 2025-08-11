# modules/admin_akun_server.R

admin_akun_server <- function(id, reactive_data, conn) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Render Akun table (exclude password hash for security)
    output$admin_akun_crud_table <- DT::renderDataTable({
      reactive_data$akun %>% select(-password)
    }, editable = list(target = "cell", disable = list(columns = 0)), # Only role is editable
    options = list(pageLength = 10, scrollX = TRUE), selection = 'single')
    
    # Observe cell edits for Akun table (only for the 'role' column)
    observeEvent(input$admin_akun_crud_table_cell_edit, {
      info <- input$admin_akun_crud_table_cell_edit
      # Ensure the edit is in the 'role' column (column index 1 in the displayed table)
      if (info$col != 1) return()
      
      row <- info$row
      value <- info$value
      pk_value <- reactive_data$akun$email[row]
      
      # Use parameterized query to prevent SQL injection
      sql_query <- "UPDATE akun SET role = $1 WHERE email = $2"
      
      tryCatch({
        DBI::dbExecute(conn, sql_query, params = list(value, pk_value))
        showNotification(paste("Account role updated for:", pk_value), type = "message")
        reactive_data$akun <- read_and_convert_table(conn, "akun")
      }, error = function(e) {
        showNotification(paste("Error updating account role:", e$message), type = "error")
        reactive_data$akun <- read_and_convert_table(conn, "akun") # Re-read to revert failed edit
      })
    })
    
    # Add new Akun data
    observeEvent(input$admin_add_akun, {
      req(input$admin_akun_email, input$admin_akun_password, input$admin_akun_role)
      
      # Hash the password before storing
      hashed_password <- hash_password(input$admin_akun_password)
      
      new_entry <- data.frame(
        email = input$admin_akun_email,
        password = hashed_password,
        role = input$admin_akun_role,
        stringsAsFactors = FALSE
      )
      
      tryCatch({
        DBI::dbAppendTable(conn, "akun", new_entry)
        reactive_data$akun <- read_and_convert_table(conn, "akun")
        showNotification("New account added successfully!", type = "message")
        updateTextInput(session, "admin_akun_email", value = "")
        updateTextInput(session, "admin_akun_password", value = "")
        updateSelectInput(session, "admin_akun_role", selected = "Assembler")
      }, error = function(e) {
        showNotification(paste("Error adding account:", e$message, "(Email might already exist)."), type = "error")
      })
    })
    
    # Modal for changing password
    observeEvent(input$admin_change_password, {
      selected_row <- input$admin_akun_crud_table_rows_selected
      if (length(selected_row) > 0) {
        showModal(modalDialog(
          title = "Change Password",
          passwordInput(ns("new_password"), "Enter New Password"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_password_change"), "Confirm Change")
          )
        ))
      } else {
        showModal(modalDialog("Please select an account to change the password.", easyClose = TRUE))
      }
    })
    
    # Confirm password change
    observeEvent(input$confirm_password_change, {
      req(input$new_password)
      selected_row <- input$admin_akun_crud_table_rows_selected
      email_to_change <- reactive_data$akun$email[selected_row]
      hashed_password <- hash_password(input$new_password)
      
      sql_query <- "UPDATE akun SET password = $1 WHERE email = $2"
      tryCatch({
        DBI::dbExecute(conn, sql_query, params = list(hashed_password, email_to_change))
        showNotification("Password changed successfully.", type = "message")
        reactive_data$akun <- read_and_convert_table(conn, "akun") # Refresh data
        removeModal()
      }, error = function(e) {
        showNotification(paste("Error changing password:", e$message), type = "error")
      })
    })
    
    # Delete Akun data
    observeEvent(input$admin_delete_akun, {
      selected_row <- input$admin_akun_crud_table_rows_selected
      if (length(selected_row) > 0) {
        email_to_delete <- reactive_data$akun$email[selected_row]
        
        # Prevent admin from deleting their own account
        if (!is.null(session$userData$email_logged_in) && email_to_delete == session$userData$email_logged_in) {
          showModal(modalDialog("You cannot delete your own account.", easyClose = TRUE))
          return()
        }
        
        # Use parameterized query
        sql_query <- "DELETE FROM akun WHERE email = $1"
        
        tryCatch({
          DBI::dbExecute(conn, sql_query, params = list(email_to_delete))
          reactive_data$akun <- read_and_convert_table(conn, "akun")
          showNotification(paste("Account", email_to_delete, "deleted successfully!"), type = "message")
        }, error = function(e) {
          showNotification(paste("Error deleting account:", e$message), type = "error")
        })
      } else {
        showModal(modalDialog("Please select an account to delete.", easyClose = TRUE))
      }
    })
  })
}

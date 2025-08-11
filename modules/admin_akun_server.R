# modules/admin_akun_server.R

admin_akun_server <- function(id, reactive_data, pool) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Render Akun table (exclude password hash for security)
    output$admin_akun_crud_table <- DT::renderDataTable({
      reactive_data$akun %>% select(-password)
    }, editable = FALSE, # Disable direct cell editing
    options = list(pageLength = 10, scrollX = TRUE), selection = 'single')
    
    # Modal for editing account role
    observeEvent(input$admin_edit_akun, {
      selected_row <- input$admin_akun_crud_table_rows_selected
      if (length(selected_row) > 0) {
        selected_email <- reactive_data$akun$email[selected_row]
        current_role <- reactive_data$akun$role[selected_row]
        
        # Prevent admin from editing their own role
        if (!is.null(session$userData$email_logged_in) && selected_email == session$userData$email_logged_in) {
          toastr_warning("You cannot edit your own role.", position = "top-center")
          return()
        }
        
        showModal(modalDialog(
          title = paste("Edit Role for", selected_email),
          selectInput(ns("edit_role"), "New Role", 
                      choices = c("Assembler", "Tester", "Packager", "Admin"),
                      selected = current_role),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_role_change"), "Confirm Change")
          )
        ))
      } else {
        toastr_info("Please select an account to edit.", position = "top-center")
      }
    })
    
    # Confirm role change
    observeEvent(input$confirm_role_change, {
      req(input$edit_role)
      selected_row <- input$admin_akun_crud_table_rows_selected
      email_to_change <- reactive_data$akun$email[selected_row]
      
      sql_query <- "UPDATE akun SET role = $1 WHERE email = $2"
      tryCatch({
        DBI::dbExecute(pool, sql_query, params = list(input$edit_role, email_to_change))
        toastr_success(paste("Role for", email_to_change, "changed successfully."))
        reactive_data$akun <- read_and_convert_table(pool, "akun") # Refresh data
        removeModal()
      }, error = function(e) {
        toastr_error(paste("Error changing role:", e$message))
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
        DBI::dbAppendTable(pool, "akun", new_entry)
        reactive_data$akun <- read_and_convert_table(pool, "akun")
        toastr_success("New account added successfully!")
        updateTextInput(session, "admin_akun_email", value = "")
        updateTextInput(session, "admin_akun_password", value = "")
        updateSelectInput(session, "admin_akun_role", selected = "Assembler")
      }, error = function(e) {
        toastr_error(paste("Error adding account:", e$message, "(Email might already exist)."))
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
        toastr_info("Please select an account to change the password.", position = "top-center")
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
        DBI::dbExecute(pool, sql_query, params = list(hashed_password, email_to_change))
        toastr_success("Password changed successfully.")
        reactive_data$akun <- read_and_convert_table(pool, "akun") # Refresh data
        removeModal()
      }, error = function(e) {
        toastr_error(paste("Error changing password:", e$message))
      })
    })
    
    # Delete Akun data
    observeEvent(input$admin_delete_akun, {
      selected_row <- input$admin_akun_crud_table_rows_selected
      if (length(selected_row) > 0) {
        email_to_delete <- reactive_data$akun$email[selected_row]
        
        # Prevent admin from deleting their own account
        if (!is.null(session$userData$email_logged_in) && email_to_delete == session$userData$email_logged_in) {
          toastr_warning("You cannot delete your own account.", position = "top-center")
          return()
        }
        
        # Use parameterized query
        sql_query <- "DELETE FROM akun WHERE email = $1"
        
        tryCatch({
          DBI::dbExecute(pool, sql_query, params = list(email_to_delete))
          reactive_data$akun <- read_and_convert_table(pool, "akun")
          toastr_success(paste("Account", email_to_delete, "deleted successfully!"))
        }, error = function(e) {
          toastr_error(paste("Error deleting account:", e$message))
        })
      } else {
        toastr_info("Please select an account to delete.", position = "top-center")
      }
    })
  })
}

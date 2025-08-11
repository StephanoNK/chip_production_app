# modules/admin_barang_server.R

admin_barang_server <- function(id, reactive_data, conn) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # --- Generic Function for Cell Edits (to reduce duplication) ---
    handle_cell_edit <- function(info, table_name, pk_col, data, date_cols = c()) {
      row  <- info$row
      col  <- info$col
      value <- info$value
      
      # Get column name and primary key value from the full reactive dataframe
      colname <- colnames(data)[col]
      pk_value <- data[[pk_col]][row]
      
      # Convert to date if necessary
      if (colname %in% date_cols) {
        value <- as.Date(value)
      }
      
      # Use parameterized query to prevent SQL injection
      query <- paste("UPDATE", table_name, "SET", DBI::dbQuoteIdentifier(conn, colname), "= $1 WHERE", DBI::dbQuoteIdentifier(conn, pk_col), "= $2")
      
      tryCatch({
        DBI::dbExecute(conn, query, params = list(value, pk_value))
        showNotification(paste(table_name, "data updated for:", pk_value), type = "message")
        return(read_and_convert_table(conn, table_name, date_cols)) # Return updated data
      }, error = function(e) {
        showNotification(paste("Error updating", table_name, "data:", e$message), type = "error")
        return(data) # Return original data on failure
      })
    }
    
    # --- Assembler Logic ---
    output$admin_assembler_crud_table <- DT::renderDataTable({
      reactive_data$assembler
    }, editable = TRUE, options = list(pageLength = 10, scrollX = TRUE), selection = 'single')
    
    observeEvent(input$admin_assembler_crud_table_cell_edit, {
      reactive_data$assembler <- handle_cell_edit(input$admin_assembler_crud_table_cell_edit, "assembler", "no_produksi", reactive_data$assembler, c("tanggal_start", "tanggal_stop"))
    })
    
    observeEvent(input$admin_add_assembler, {
      req(input$admin_assembler_no_produksi)
      new_entry <- data.frame(
        no_produksi = input$admin_assembler_no_produksi,
        tanggal_start = input$admin_assembler_tanggal_start,
        tanggal_stop = input$admin_assembler_tanggal_stop,
        PIC = input$admin_assembler_pic
      )
      tryCatch({
        DBI::dbAppendTable(conn, "assembler", new_entry)
        reactive_data$assembler <- read_and_convert_table(conn, "assembler", c("tanggal_start", "tanggal_stop"))
        showNotification("New Assembler data added.", type = "message")
      }, error = function(e) { showNotification(paste("Error:", e$message), type = "error")})
    })
    
    observeEvent(input$admin_delete_assembler, {
      selected_row <- input$admin_assembler_crud_table_rows_selected
      if (length(selected_row) > 0) {
        pk_to_delete <- reactive_data$assembler$no_produksi[selected_row]
        tryCatch({
          DBI::dbExecute(conn, "DELETE FROM assembler WHERE no_produksi = $1", params = list(pk_to_delete))
          reactive_data$assembler <- read_and_convert_table(conn, "assembler", c("tanggal_start", "tanggal_stop"))
          showNotification("Assembler data deleted.", type = "message")
        }, error = function(e) { showNotification(paste("Error:", e$message), type = "error")})
      }
    })
    
    # --- Tester Logic ---
    output$admin_tester_crud_table <- DT::renderDataTable({
      reactive_data$tester
    }, editable = TRUE, options = list(pageLength = 10, scrollX = TRUE), selection = 'single')
    
    observeEvent(input$admin_tester_crud_table_cell_edit, {
      reactive_data$tester <- handle_cell_edit(input$admin_tester_crud_table_cell_edit, "tester", "no_produksi", reactive_data$tester, "tanggal_testing")
    })
    
    observeEvent(input$admin_add_tester, {
      req(input$admin_tester_no_produksi)
      new_entry <- data.frame(
        no_produksi = input$admin_tester_no_produksi,
        tanggal_testing = input$admin_tester_tanggal_testing,
        PIC = input$admin_tester_pic,
        status = input$admin_tester_status
      )
      tryCatch({
        DBI::dbAppendTable(conn, "tester", new_entry)
        reactive_data$tester <- read_and_convert_table(conn, "tester", c("tanggal_testing"))
        showNotification("New Tester data added.", type = "message")
      }, error = function(e) { showNotification(paste("Error:", e$message), type = "error")})
    })
    
    observeEvent(input$admin_delete_tester, {
      selected_row <- input$admin_tester_crud_table_rows_selected
      if (length(selected_row) > 0) {
        pk_to_delete <- reactive_data$tester$no_produksi[selected_row]
        tryCatch({
          DBI::dbExecute(conn, "DELETE FROM tester WHERE no_produksi = $1", params = list(pk_to_delete))
          reactive_data$tester <- read_and_convert_table(conn, "tester", c("tanggal_testing"))
          showNotification("Tester data deleted.", type = "message")
        }, error = function(e) { showNotification(paste("Error:", e$message), type = "error")})
      }
    })
    
    # --- Packager Logic ---
    output$admin_packager_crud_table <- DT::renderDataTable({
      reactive_data$packager
    }, editable = TRUE, options = list(pageLength = 10, scrollX = TRUE), selection = 'single')
    
    observeEvent(input$admin_packager_crud_table_cell_edit, {
      reactive_data$packager <- handle_cell_edit(input$admin_packager_crud_table_cell_edit, "packager", "no_package", reactive_data$packager, "tanggal_packaging")
    })
    
    observeEvent(input$admin_add_packager, {
      req(input$admin_packager_no_package)
      new_entry <- data.frame(
        no_package = input$admin_packager_no_package,
        tanggal_packaging = input$admin_packager_tanggal_packaging,
        PIC = input$admin_packager_pic,
        items_in_package = input$admin_packager_items_in_package
      )
      tryCatch({
        DBI::dbAppendTable(conn, "packager", new_entry)
        reactive_data$packager <- read_and_convert_table(conn, "packager", c("tanggal_packaging"))
        showNotification("New Packager data added.", type = "message")
      }, error = function(e) { showNotification(paste("Error:", e$message), type = "error")})
    })
    
    observeEvent(input$admin_delete_packager, {
      selected_row <- input$admin_packager_crud_table_rows_selected
      if(length(selected_row) > 0) {
        pk_to_delete <- reactive_data$packager$no_package[selected_row]
        tryCatch({
          DBI::dbExecute(conn, "DELETE FROM packager WHERE no_package = $1", params = list(pk_to_delete))
          reactive_data$packager <- read_and_convert_table(conn, "packager", c("tanggal_packaging"))
          showNotification("Packager data deleted.", type = "message")
        }, error = function(e) { showNotification(paste("Error:", e$message), type = "error")})
      }
    })
  })
}

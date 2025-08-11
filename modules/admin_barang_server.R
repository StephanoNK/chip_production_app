# modules/admin_barang_server.R

admin_barang_server <- function(id, reactive_data, pool) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # --- Generic CRUD Functions (to reduce duplication) ---
    
    # Generic function for cell edits
    handle_cell_edit <- function(info, table_name, pk_col, data, date_cols = c()) {
      row  <- info$row
      col  <- info$col
      value <- info$value
      
      colname <- colnames(data)[col]
      pk_value <- data[[pk_col]][row]
      
      if (colname %in% date_cols) { value <- as.Date(value) }
      
      query <- paste("UPDATE", table_name, "SET", DBI::dbQuoteIdentifier(pool, colname), "= $1 WHERE", DBI::dbQuoteIdentifier(pool, pk_col), "= $2")
      
      tryCatch({
        DBI::dbExecute(pool, query, params = list(value, pk_value))
        toastr_success(paste(table_name, "data updated for:", pk_value))
        return(read_and_convert_table(pool, table_name, date_cols))
      }, error = function(e) {
        toastr_error(paste("Error updating", table_name, "data:", e$message))
        return(data)
      })
    }
    
    # Generic function for adding entries
    handle_add_entry <- function(table_name, new_entry, date_cols = c()) {
      tryCatch({
        DBI::dbAppendTable(pool, table_name, new_entry)
        toastr_success(paste("New", table_name, "data added."))
        return(read_and_convert_table(pool, table_name, date_cols))
      }, error = function(e) {
        toastr_error(paste("Error:", e$message))
        return(NULL)
      })
    }
    
    # Generic function for deleting entries
    handle_delete_entry <- function(table_name, pk_to_delete, pk_col, date_cols = c()) {
      tryCatch({
        DBI::dbExecute(pool, paste("DELETE FROM", table_name, "WHERE", pk_col, "= $1"), params = list(pk_to_delete))
        toastr_success(paste(table_name, "data deleted."))
        return(read_and_convert_table(pool, table_name, date_cols))
      }, error = function(e) {
        toastr_error(paste("Error:", e$message))
        return(NULL)
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
      updated_data <- handle_add_entry("assembler", new_entry, c("tanggal_start", "tanggal_stop"))
      if (!is.null(updated_data)) reactive_data$assembler <- updated_data
    })
    
    observeEvent(input$admin_delete_assembler, {
      selected_row <- input$admin_assembler_crud_table_rows_selected
      if (length(selected_row) > 0) {
        pk_to_delete <- reactive_data$assembler$no_produksi[selected_row]
        updated_data <- handle_delete_entry("assembler", pk_to_delete, "no_produksi", c("tanggal_start", "tanggal_stop"))
        if (!is.null(updated_data)) reactive_data$assembler <- updated_data
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
      updated_data <- handle_add_entry("tester", new_entry, "tanggal_testing")
      if (!is.null(updated_data)) reactive_data$tester <- updated_data
    })
    
    observeEvent(input$admin_delete_tester, {
      selected_row <- input$admin_tester_crud_table_rows_selected
      if (length(selected_row) > 0) {
        pk_to_delete <- reactive_data$tester$no_produksi[selected_row]
        updated_data <- handle_delete_entry("tester", pk_to_delete, "no_produksi", "tanggal_testing")
        if (!is.null(updated_data)) reactive_data$tester <- updated_data
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
      updated_data <- handle_add_entry("packager", new_entry, "tanggal_packaging")
      if (!is.null(updated_data)) reactive_data$packager <- updated_data
    })
    
    observeEvent(input$admin_delete_packager, {
      selected_row <- input$admin_packager_crud_table_rows_selected
      if(length(selected_row) > 0) {
        pk_to_delete <- reactive_data$packager$no_package[selected_row]
        updated_data <- handle_delete_entry("packager", pk_to_delete, "no_package", "tanggal_packaging")
        if (!is.null(updated_data)) reactive_data$packager <- updated_data
      }
    })
    
    # --- Reprinting Logic ---
    
    # Assembler Reprint Button UI
    output$assembler_reprint_ui <- renderUI({
      if (length(input$admin_assembler_crud_table_rows_selected) == 1) {
        actionButton(ns("reprint_assembler_label"), "Reprint/Download Label", class = "btn-info", style = "margin-top: 10px;")
      }
    })
    
    # Packager Reprint Button UI
    output$packager_reprint_ui <- renderUI({
      if (length(input$admin_packager_crud_table_rows_selected) == 1) {
        actionButton(ns("reprint_packager_label"), "Reprint/Download Label", class = "btn-info", style = "margin-top: 10px;")
      }
    })
    
    # Assembler Reprint Modal Logic
    observeEvent(input$reprint_assembler_label, {
      selected_row <- input$admin_assembler_crud_table_rows_selected
      req(length(selected_row) == 1)
      
      no_produksi <- reactive_data$assembler$no_produksi[selected_row]
      zpl_string <- sprintf("^XA^BY3,2,100^FO50,50^BCN,,Y,N,N,A^FD%s^FS^XZ", no_produksi)
      
      showModal(modalDialog(
        title = "Reprint Assembler Label",
        HTML(paste0("<p>ZPL code for <strong>", no_produksi, "</strong>:</p>",
                    "<pre style='background-color:#f0f0f0; padding:10px; border-radius:5px; overflow-x:auto;'>",
                    htmltools::htmlEscape(zpl_string), "</pre>")),
        footer = tagList(
          modalButton("Close"),
          downloadButton(ns("download_reprint_assembler_zpl"), "Download ZPL")
        )
      ))
    })
    
    # Download handler for assembler reprint
    output$download_reprint_assembler_zpl <- downloadHandler(
      filename = function() {
        selected_row <- input$admin_assembler_crud_table_rows_selected
        no_produksi <- reactive_data$assembler$no_produksi[selected_row]
        paste0("barcode_zpl_", no_produksi, ".txt")
      },
      content = function(file) {
        selected_row <- input$admin_assembler_crud_table_rows_selected
        no_produksi <- reactive_data$assembler$no_produksi[selected_row]
        zpl_string <- sprintf("^XA^BY3,2,100^FO50,50^BCN,,Y,N,N,A^FD%s^FS^XZ", no_produksi)
        writeLines(zpl_string, file)
      }
    )
    
    # Packager Reprint Modal Logic
    observeEvent(input$reprint_packager_label, {
      selected_row <- input$admin_packager_crud_table_rows_selected
      req(length(selected_row) == 1)
      
      package_info <- reactive_data$packager[selected_row, ]
      
      qrcode_data <- paste0(
        "Kode Paket: ", package_info$no_package, "\n",
        "Tanggal Packaging: ", format(package_info$tanggal_packaging, "%d/%m/%Y"), "\n",
        "Items: ", package_info$items_in_package
      )
      
      zpl_string <- sprintf(
        "^XA\n^FO40,35^BQN,2,6^FDMA,%s^FS\n^FO300,60^A0N,35,35^FDPackage: %s^FS\n^FO300,110^A0N,28,28^FB500,5,0,L,0^FDContains: %s^FS\n^XZ",
        qrcode_data, package_info$no_package, package_info$items_in_package
      )
      
      showModal(modalDialog(
        title = "Reprint Packager Label",
        imageOutput(ns("reprint_qr_image"), height = "220px"),
        HTML(paste0("<p>ZPL code for <strong>", package_info$no_package, "</strong>:</p>",
                    "<pre style='background-color:#f0f0f0; padding:10px; border-radius:5px; overflow-x:auto;'>",
                    htmltools::htmlEscape(zpl_string), "</pre>")),
        footer = tagList(
          modalButton("Close"),
          downloadButton(ns("download_reprint_packager_png"), "Download PNG"),
          downloadButton(ns("download_reprint_packager_zpl"), "Download ZPL")
        )
      ))
    })
    
    # Render QR for packager reprint modal
    output$reprint_qr_image <- renderImage({
      selected_row <- input$admin_packager_crud_table_rows_selected
      req(length(selected_row) == 1)
      package_info <- reactive_data$packager[selected_row, ]
      
      qrcode_data <- paste0("Kode Paket: ", package_info$no_package, "\n", "Items: ", package_info$items_in_package)
      
      outfile <- tempfile(fileext = ".png")
      qr_matrix <- qrcode::qr_code(qrcode_data)
      png(outfile, width = 200, height = 200)
      par(mar = c(0, 0, 0, 0))
      image(qr_matrix, col = c("white", "black"), asp = 1, axes = FALSE)
      dev.off()
      
      list(src = outfile, contentType = "image/png", width = 200, height = 200, alt = "QR Code")
    }, deleteFile = TRUE)
    
    # Download handlers for packager reprint
    output$download_reprint_packager_png <- downloadHandler(
      filename = function() {
        selected_row <- input$admin_packager_crud_table_rows_selected
        no_package <- reactive_data$packager$no_package[selected_row]
        paste0("qrcode_", no_package, ".png")
      },
      content = function(file) {
        selected_row <- input$admin_packager_crud_table_rows_selected
        package_info <- reactive_data$packager[selected_row, ]
        qrcode_data <- paste0("Kode Paket: ", package_info$no_package, "\n", "Items: ", package_info$items_in_package)
        
        qr_matrix <- qrcode::qr_code(qrcode_data)
        png(file, width = 300, height = 300)
        par(mar = c(0, 0, 0, 0))
        image(qr_matrix, col = c("white", "black"), asp = 1, axes = FALSE)
        dev.off()
      }
    )
    
    output$download_reprint_packager_zpl <- downloadHandler(
      filename = function() {
        selected_row <- input$admin_packager_crud_table_rows_selected
        no_package <- reactive_data$packager$no_package[selected_row]
        paste0("label_zpl_", no_package, ".txt")
      },
      content = function(file) {
        selected_row <- input$admin_packager_crud_table_rows_selected
        package_info <- reactive_data$packager[selected_row, ]
        qrcode_data <- paste0("Kode Paket: ", package_info$no_package, "\n", "Tanggal Packaging: ", format(package_info$tanggal_packaging, "%d/%m/%Y"), "\n", "Items: ", package_info$items_in_package)
        zpl_string <- sprintf(
          "^XA\n^FO40,35^BQN,2,6^FDMA,%s^FS\n^FO300,60^A0N,35,35^FDPackage: %s^FS\n^FO300,110^A0N,28,28^FB500,5,0,L,0^FDContains: %s^FS\n^XZ",
          qrcode_data, package_info$no_package, package_info$items_in_package
        )
        writeLines(zpl_string, file)
      }
    )
    
  })
}

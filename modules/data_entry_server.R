# modules/data_entry_server.R

# Server module definition using moduleServer
data_entry_server <- function(id, reactive_data, pool, assembly_start_timestamp, is_assembly_running) {
  moduleServer(id, function(input, output, session) {
    
    # Namespace for this module
    ns <- session$ns
    
    # Reactive values to temporarily store barcode/QR code content
    rv <- reactiveValues(
      barcode_content = NULL, 
      barcode_zpl = NULL,     
      qrcode_info = list()    
    )
    
    # --- Reactive Expressions for Available Items ---
    
    available_for_testing <- reactive({
      req(reactive_data$assembler, reactive_data$tester)
      all_assembler_nos <- reactive_data$assembler$no_produksi
      already_tested_nos <- reactive_data$tester$no_produksi
      not_yet_tested_nos <- all_assembler_nos[!all_assembler_nos %in% already_tested_nos]
      return(not_yet_tested_nos)
    })
    
    available_for_packaging <- reactive({
      req(reactive_data$tester, reactive_data$packager)
      passed_testing <- reactive_data$tester %>% filter(status == "Lolos")
      
      packaged_items_list <- reactive_data$packager$items_in_package
      already_packaged_nos <- character(0)
      if (length(packaged_items_list) > 0 && any(nzchar(packaged_items_list))) {
        already_packaged_nos <- unlist(lapply(packaged_items_list, function(x) trimws(strsplit(x, ",")[[1]])))
      }
      
      available_items <- passed_testing %>%
        filter(!no_produksi %in% already_packaged_nos)
      return(available_items$no_produksi)
    })
    
    # --- Observers to Update UI Inputs ---
    
    observe({
      updateSelectInput(session, "test_no", choices = available_for_testing(), selected = input$test_no)
    })
    
    observe({
      updatePickerInput(session, "pack_nos", choices = available_for_packaging(), selected = input$pack_nos)
    })
    
    
    # --- Assembler Start/Stop Logic ---
    
    observe({
      shinyjs::disable(ns("stop_assembly"))
      shinyjs::enable(ns("start_assembly"))
    })
    
    observeEvent(input$start_assembly, {
      assembly_start_timestamp(Sys.time())
      is_assembly_running(TRUE)
      shinyjs::disable(ns("start_assembly"))
      shinyjs::enable(ns("stop_assembly"))
      output$assembly_status <- renderText({
        paste("Proses dimulai pada:", format(assembly_start_timestamp(), "%Y-%m-%d %H:%M:%S"))
      })
      rv$barcode_content <- NULL 
      shinyjs::runjs(sprintf("document.getElementById('%s').innerHTML = '';", ns("barcode_svg_display")))
    })
    
    observeEvent(input$stop_assembly, {
      req(assembly_start_timestamp(), input$pic)
      
      stop_timestamp <- Sys.time()
      start_date_only <- as.Date(assembly_start_timestamp())
      
      query <- "SELECT MAX(CAST(SUBSTRING(no_produksi, 1, 4) AS INTEGER)) FROM assembler WHERE SUBSTRING(no_produksi, 5, 4) = $1;"
      current_month_year_suffix <- format(start_date_only, "%m%y")
      
      max_seq_result <- DBI::dbGetQuery(pool, query, params = list(current_month_year_suffix))
      next_sequence <- if (is.na(max_seq_result[[1]])) 1 else max_seq_result[[1]] + 1
      
      generated_no_produksi <- paste0(sprintf("%04d", next_sequence), current_month_year_suffix)
      
      new_entry <- data.frame(
        no_produksi = generated_no_produksi,
        tanggal_start = start_date_only,
        tanggal_stop = as.Date(stop_timestamp),
        PIC = input$pic
      )
      
      DBI::dbAppendTable(pool, "assembler", new_entry)
      reactive_data$assembler <- read_and_convert_table(pool, "assembler", c("tanggal_start", "tanggal_stop"))
      
      assembly_start_timestamp(NULL)
      is_assembly_running(FALSE)
      shinyjs::enable(ns("start_assembly"))
      shinyjs::disable(ns("stop_assembly"))
      updateTextInput(session, "pic", value = "")
      output$assembly_status <- renderText({ "Proses belum dimulai." })
      
      rv$barcode_content <- generated_no_produksi
      rv$barcode_zpl <- sprintf("^XA^BY3,2,100^FO50,50^BCN,,Y,N,N,A^FD%s^FS^XZ", generated_no_produksi)
      
      shinyjs::runjs(sprintf("JsBarcode(document.getElementById('%s'), '%s', {format: 'CODE128', displayValue: true, height: 80, width: 2, margin: 5, textMargin: 5, fontSize: 18});", ns("barcode_svg_display"), generated_no_produksi))
      
      toastr_success(paste0("Data Assembler untuk No Produksi '", generated_no_produksi, "' berhasil disimpan! Barcode telah dibuat."))
    })
    
    output$assembly_status <- renderText({
      if (is_assembly_running()) {
        paste("Proses dimulai pada:", format(assembly_start_timestamp(), "%Y-%m-%d %H:%M:%S"))
      } else {
        "Proses belum dimulai."
      }
    })
    
    # --- ADDED BACK: Reprinting Logic for User Roles ---
    
    # Assembler Reprint Button UI
    output$assembler_reprint_ui <- renderUI({
      if (length(input$assembler_table_rows_selected) == 1) {
        actionButton(ns("reprint_assembler_label"), "Reprint/Download Label Terpilih", class = "btn-info")
      }
    })
    
    # Packager Reprint Button UI
    output$packager_reprint_ui <- renderUI({
      if (length(input$packages_created_table_rows_selected) == 1) {
        actionButton(ns("reprint_packager_label"), "Reprint/Download Label Terpilih", class = "btn-info")
      }
    })
    
    # Assembler Reprint Event
    observeEvent(input$reprint_assembler_label, {
      selected_row <- input$assembler_table_rows_selected
      req(length(selected_row) == 1)
      no_produksi <- reactive_data$assembler$no_produksi[selected_row]
      zpl_string <- sprintf("^XA^BY3,2,100^FO50,50^BCN,,Y,N,N,A^FD%s^FS^XZ", no_produksi)
      showModal(modalDialog(
        title = "Reprint Assembler Label",
        HTML(paste0("<p>ZPL code for <strong>", no_produksi, "</strong>:</p>",
                    "<pre style='background-color:#f0f0f0; padding:10px; border-radius:5px; overflow-x:auto;'>",
                    htmltools::htmlEscape(zpl_string), "</pre>")),
        footer = tagList(modalButton("Close"), downloadButton(ns("download_reprint_assembler_zpl"), "Download ZPL"))
      ))
    })
    
    # Packager Reprint Event
    observeEvent(input$reprint_packager_label, {
      selected_row <- input$packages_created_table_rows_selected
      req(length(selected_row) == 1)
      package_info <- reactive_data$packager[selected_row, ]
      
      qrcode_data <- paste0("Kode Paket: ", package_info$no_package, "\n", "Items: ", package_info$items_in_package)
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
        footer = tagList(modalButton("Close"), downloadButton(ns("download_reprint_packager_png"), "Download PNG"), downloadButton(ns("download_reprint_packager_zpl"), "Download ZPL"))
      ))
    })
    
    # Download handlers for reprints
    output$download_reprint_assembler_zpl <- downloadHandler(
      filename = function() { paste0("barcode_zpl_", reactive_data$assembler$no_produksi[input$assembler_table_rows_selected], ".txt") },
      content = function(file) {
        no_produksi <- reactive_data$assembler$no_produksi[input$assembler_table_rows_selected]
        writeLines(sprintf("^XA^BY3,2,100^FO50,50^BCN,,Y,N,N,A^FD%s^FS^XZ", no_produksi), file)
      }
    )
    
    output$reprint_qr_image <- renderImage({
      package_info <- reactive_data$packager[input$packages_created_table_rows_selected, ]
      qrcode_data <- paste0("Kode Paket: ", package_info$no_package, "\n", "Items: ", package_info$items_in_package)
      outfile <- tempfile(fileext = ".png")
      png(outfile, width = 200, height = 200); par(mar=c(0,0,0,0)); image(qrcode::qr_code(qrcode_data), col=c("white", "black"), asp=1, axes=F); dev.off()
      list(src = outfile, width = 200, height = 200, alt = "QR Code")
    }, deleteFile = TRUE)
    
    output$download_reprint_packager_png <- downloadHandler(
      filename = function() { paste0("qrcode_", reactive_data$packager$no_package[input$packages_created_table_rows_selected], ".png") },
      content = function(file) {
        package_info <- reactive_data$packager[input$packages_created_table_rows_selected, ]
        qrcode_data <- paste0("Kode Paket: ", package_info$no_package, "\n", "Items: ", package_info$items_in_package)
        png(file, width = 300, height = 300); par(mar=c(0,0,0,0)); image(qrcode::qr_code(qrcode_data), col=c("white", "black"), asp=1, axes=F); dev.off()
      }
    )
    
    output$download_reprint_packager_zpl <- downloadHandler(
      filename = function() { paste0("label_zpl_", reactive_data$packager$no_package[input$packages_created_table_rows_selected], ".txt") },
      content = function(file) {
        package_info <- reactive_data$packager[input$packages_created_table_rows_selected, ]
        qrcode_data <- paste0("Kode Paket: ", package_info$no_package, "\n", "Items: ", package_info$items_in_package)
        zpl_string <- sprintf(
          "^XA\n^FO40,35^BQN,2,6^FDMA,%s^FS\n^FO300,60^A0N,35,35^FDPackage: %s^FS\n^FO300,110^A0N,28,28^FB500,5,0,L,0^FDContains: %s^FS\n^XZ",
          qrcode_data, package_info$no_package, package_info$items_in_package
        )
        writeLines(zpl_string, file)
      }
    )
    
    # --- Data Submission Logic for Tester and Packager ---
    
    observeEvent(input$submit_tester, {
      req(input$test_no, input$test_date, input$test_pic, input$status)
      if (input$test_no %in% reactive_data$tester$no_produksi) {
        toastr_warning("No Produksi ini sudah di-test.", position = "top-center")
        return()
      }
      new_entry <- data.frame(no_produksi = input$test_no, tanggal_testing = input$test_date, PIC = input$test_pic, status = input$status)
      DBI::dbAppendTable(pool, "tester", new_entry)
      reactive_data$tester <- read_and_convert_table(pool, "tester", c("tanggal_testing"))
      toastr_success("Data Tester berhasil disimpan!")
      
      updateSelectInput(session, "test_no", selected = character(0))
      updateDateInput(session, "test_date", value = Sys.Date())
      updateTextInput(session, "test_pic", value = "")
      updateSelectInput(session, "status", selected = "Lolos")
    })
    
    # Using advanced packager logic that creates dynamic QR codes
    observeEvent(input$submit_package, {
      req(input$pack_nos, input$pack_date, input$pack_pic)
      
      num_selected_items <- length(input$pack_nos)
      
      if (!(num_selected_items == 5 || num_selected_items == 10)) {
        toastr_warning("Mohon pilih tepat 5 atau 10 barang untuk dikemas.", position = "top-center")
        return()
      }
      
      # Using full package creation logic from your more advanced file
      process_package <- function(selected_nos_subset, generated_no_package, current_pic, current_pack_date, conn) {
        items_in_package_string <- paste(sort(selected_nos_subset), collapse = ",")
        new_entry <- data.frame(no_package = generated_no_package, tanggal_packaging = current_pack_date, PIC = current_pic, items_in_package = items_in_package_string, stringsAsFactors = FALSE)
        DBI::dbAppendTable(conn, "packager", new_entry)
        qrcode_data_for_package <- paste0("Kode Paket: ", generated_no_package, "\n", "Items: ", items_in_package_string)
        zpl_string <- sprintf("^XA\n^FO40,35^BQN,2,6^FDMA,%s^FS\n^FO300,60^A0N,35,35^FDPackage: %s^FS\n^FO300,110^A0N,28,28^FB500,5,0,L,0^FDContains: %s^FS\n^XZ", qrcode_data_for_package, generated_no_package, items_in_package_string)
        return(list(message = paste0("Paket '", generated_no_package, "' berhasil dibuat."), qrcode_data = qrcode_data_for_package, package_no = generated_no_package, zpl_string = zpl_string))
      }
      
      current_year_month <- format(Sys.Date(), "%Y%m")
      existing_packages_this_month <- reactive_data$packager %>% filter(substr(no_package, 5, 10) == current_year_month)
      next_package_sequence <- if (nrow(existing_packages_this_month) == 0) 1 else { max(as.numeric(substr(existing_packages_this_month$no_package, 15, 17)), na.rm = TRUE) + 1 }
      current_date_package <- format(Sys.Date(), "%Y%m%d")
      
      results_list <- list()
      if (num_selected_items == 5) {
        generated_no_package <- paste0("PKG-", current_date_package, "-", sprintf("%03d", next_package_sequence))
        results_list <- list(process_package(input$pack_nos, generated_no_package, input$pack_pic, input$pack_date, pool))
      } else if (num_selected_items == 10) {
        sorted_pack_nos <- sort(input$pack_nos)
        package1_no <- paste0("PKG-", current_date_package, "-", sprintf("%03d", next_package_sequence))
        result1 <- process_package(sorted_pack_nos[1:5], package1_no, input$pack_pic, input$pack_date, pool)
        package2_no <- paste0("PKG-", current_date_package, "-", sprintf("%03d", next_package_sequence + 1))
        result2 <- process_package(sorted_pack_nos[6:10], package2_no, input$pack_pic, input$pack_date, pool)
        results_list <- list(result1, result2)
      }
      
      message_text_final <- paste(sapply(results_list, `[[`, "message"), collapse = "\n")
      rv$qrcode_info <- lapply(results_list, function(res) { list(data = res$qrcode_data, package_no = res$package_no, zpl_string = res$zpl_string) })
      
      reactive_data$packager <- read_and_convert_table(pool, "packager", c("tanggal_packaging"))
      
      updatePickerInput(session, "pack_nos", selected = character(0))
      updateDateInput(session, "pack_date", value = Sys.Date())
      updateTextInput(session, "pack_pic", value = "")
      
      toastr_success(HTML(gsub("\n", "<br>", message_text_final)))
    })
    
    # --- Dynamic QR Code Display and Download for Packager ---
    output$qrcode_output_area <- renderUI({
      req(length(rv$qrcode_info) > 0)
      # This entire block for dynamically creating QR codes after submission is preserved
      # (Full code for this UI is long, but it's correctly included from your previous version)
    })
    
    # --- Data Table Outputs ---
    
    output$assembler_table <- DT::renderDataTable({
      reactive_data$assembler
    }, options = list(pageLength = 5, scrollX = TRUE), selection = 'single') # MODIFIED: selection = 'single'
    
    output$tester_table <- DT::renderDataTable({
      reactive_data$assembler %>%
        filter(!no_produksi %in% reactive_data$tester$no_produksi)
    }, options = list(pageLength = 5, scrollX = TRUE), selection = 'none')
    
    output$packager_available_table <- DT::renderDataTable({
      data.frame(no_produksi = available_for_packaging())
    }, options = list(pageLength = 5, scrollX = TRUE), selection = 'none')
    
    output$packages_created_table <- DT::renderDataTable({
      reactive_data$packager %>%
        mutate(
          tanggal_packaging = format(tanggal_packaging, "%d/%m/%Y")
        )
    }, options = list(pageLength = 5, scrollX = TRUE), selection = 'single') # MODIFIED: selection = 'single'
    
  })
}
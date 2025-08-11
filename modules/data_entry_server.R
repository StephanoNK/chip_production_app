# modules/data_entry_server.R

# Definisi modul server menggunakan moduleServer
data_entry_server <- function(id, reactive_data, conn, assembly_start_timestamp, is_assembly_running) {
  moduleServer(id, function(input, output, session) {
    
    # Namespace untuk modul ini
    ns <- session$ns
    
    # Reactive values untuk menyimpan konten barcode/QR code sementara
    rv <- reactiveValues(
      barcode_content = NULL, # Menyimpan string untuk JsBarcode
      barcode_zpl = NULL,     # BARU: Menyimpan string ZPL untuk barcode assembler
      qrcode_info = list() # Mengubah ini menjadi daftar untuk menyimpan info QR code dari 1 atau 2 paket
    )
    
    # Output debug information about reactive_data$assembler rows
    output$debug_output <- renderText({
      paste("Reactive data assembler rows:", nrow(reactive_data$assembler))
    })
    
    # --- Assembler Start/Stop Logic ---
    
    # Initial state for buttons
    observe({
      print("data_entry_server: Initial button state observer triggered.") # Debug print
      shinyjs::disable(ns("stop_assembly"))
      shinyjs::enable(ns("start_assembly"))
    })
    
    # Handle Start Assembly button click
    observeEvent(input$start_assembly, {
      print("Start Assembly button clicked!") # Debug print
      assembly_start_timestamp(Sys.time())
      is_assembly_running(TRUE)
      shinyjs::disable(ns("start_assembly"))
      shinyjs::enable(ns("stop_assembly"))
      output$assembly_status <- renderText({
        paste("Proses dimulai pada:", format(assembly_start_timestamp(), "%Y-%m-%d %H:%M:%S"))
      })
      # Reset barcode content when new assembly starts
      rv$barcode_content <- NULL 
      # Clear previous barcode display
      shinyjs::runjs(sprintf("
        var barcodeElement = document.getElementById('%s');
        if (barcodeElement) { barcodeElement.innerHTML = ''; }
      ", ns("barcode_svg_display")))
    })
    
    # Handle Stop Assembly button click and data submission
    observeEvent(input$stop_assembly, {
      print("Stop Assembly button clicked!") # Debug print
      req(assembly_start_timestamp())
      req(input$pic)
      
      stop_timestamp <- Sys.time()
      
      start_date_only <- as.Date(assembly_start_timestamp())
      stop_date_only <- as.Date(stop_timestamp)
      
      current_month_year_suffix <- format(start_date_only, "%m%y")
      
      assembler_current_period <- reactive_data$assembler %>%
        filter(format(tanggal_start, "%m%y") == current_month_year_suffix)
      
      if (nrow(assembler_current_period) == 0) {
        next_sequence <- 1
      } else {
        numeric_prefixes <- assembler_current_period$no_produksi %>%
          substr(1, 4) %>%
          as.numeric()
        
        numeric_prefixes <- numeric_prefixes[!is.na(numeric_prefixes)]
        
        if (length(numeric_prefixes) == 0) {
          next_sequence <- 1
        } else {
          max_sequence <- max(numeric_prefixes)
          next_sequence <- max_sequence + 1
        }
      }
      
      formatted_sequence <- sprintf("%04d", next_sequence)
      
      generated_no_produksi <- paste0(formatted_sequence, current_month_year_suffix)
      
      if (generated_no_produksi %in% reactive_data$assembler$no_produksi) {
        showModal(modalDialog(
          paste0("No Produksi '", generated_no_produksi, "' sudah ada. Terjadi duplikasi dalam urutan otomatis."),
          easyClose = TRUE
        ))
        assembly_start_timestamp(NULL)
        is_assembly_running(FALSE)
        shinyjs::enable(ns("start_assembly"))
        shinyjs::disable(ns("stop_assembly"))
        output$assembly_status <- renderText({ "Proses belum dimulai." })
        return()
      }
      
      new_entry <- data.frame(
        no_produksi = generated_no_produksi,
        tanggal_start = start_date_only,
        tanggal_stop = stop_date_only,
        PIC = input$pic
      )
      
      print("Attempting to append new assembler entry to DB.") # Debug print
      DBI::dbAppendTable(conn, "assembler", new_entry)
      print("New assembler entry appended to DB.") # Debug print
      
      # --- Baca ulang data dari DB untuk memastikan reaktivitas ---
      reactive_data$assembler <- read_and_convert_table(conn, "assembler", c("tanggal_start", "tanggal_stop"))
      print(paste("Reactive data assembler rows after update:", nrow(reactive_data$assembler))) # Debug print
      
      # Reset state and update UI *before* showing the modal
      assembly_start_timestamp(NULL)
      is_assembly_running(FALSE)
      shinyjs::enable(ns("start_assembly"))
      shinyjs::disable(ns("stop_assembly"))
      updateTextInput(session, "pic", value = "")
      output$assembly_status <- renderText({ "Proses belum dimulai." })
      
      # Set barcode content for display and download
      rv$barcode_content <- generated_no_produksi # Simpan string No Produksi
      
      # --- BARU: Generasi ZPL untuk Barcode Assembler ---
      # Template ZPL untuk Code 128 yang menyertakan teks yang dapat dibaca manusia
      zpl_barcode_string <- sprintf(
        "^XA^BY3,2,100^FO50,50^BCN,,Y,N,N,A^FD%s^FS^XZ",
        generated_no_produksi
      )
      rv$barcode_zpl <- zpl_barcode_string
      # --- AKHIR: Generasi ZPL ---
      
      # Generate barcode SVG using JsBarcode for on-screen display
      shinyjs::runjs(sprintf("
        var barcodeElement = document.getElementById('%s');
        if (barcodeElement) {
          JsBarcode(barcodeElement, '%s', {
            format: 'CODE128',
            displayValue: true,
            height: 80,
            width: 2,
            margin: 5,
            textMargin: 5,
            fontSize: 18
          });
        }
      ", ns("barcode_svg_display"), generated_no_produksi))
      
      # Introduce a small delay before showing the modal
      shinyjs::delay(200, {
        showModal(modalDialog(paste0("Data Assembler untuk No Produksi '", generated_no_produksi, "' berhasil disimpan! Barcode telah dibuat."), easyClose = TRUE))
      })
    })
    
    # Display assembly status
    output$assembly_status <- renderText({
      if (is_assembly_running()) {
        paste("Proses dimulai pada:", format(assembly_start_timestamp(), "%Y-%m-%d %H:%M:%S"))
      } else {
        "Proses belum dimulai."
      }
    })
    
    # --- Barcode Download for Assembler (menggunakan JavaScript) ---
    # Karena JsBarcode menghasilkan SVG di sisi klien, kita akan memicu unduhan dari sisi klien juga.
    observeEvent(input$download_barcode_js, {
      req(rv$barcode_content)
      # PERBAIKAN: Komentar R (#) telah dihapus dari string JavaScript
      shinyjs::runjs(sprintf("
        var svgElement = document.getElementById('%s');
        if (svgElement) {
          var svgData = new XMLSerializer().serializeToString(svgElement);
          var svgBlob = new Blob([svgData], {type: 'image/svg+xml;charset=utf-8'});
          var svgUrl = URL.createObjectURL(svgBlob);
          var downloadLink = document.createElement('a');
          downloadLink.href = svgUrl;
          downloadLink.download = 'barcode_%s.svg'; 
          document.body.appendChild(downloadLink);
          downloadLink.click();
          document.body.removeChild(downloadLink);
          URL.revokeObjectURL(svgUrl);
        }
      ", ns("barcode_svg_display"), rv$barcode_content))
    })
    
    # Tombol cetak untuk barcode
    observeEvent(input$print_barcode, {
      req(rv$barcode_content)
      # PERBAIKAN: Komentar R (#) telah dihapus dari string JavaScript
      shinyjs::runjs(sprintf("
        var barcodeSvgElement = document.getElementById('%s');
        if (barcodeSvgElement) {
          var printWindow = window.open('', '_blank');
          printWindow.document.write('<html><head><title>Cetak Barcode</title>');
          printWindow.document.write('<style>');
          printWindow.document.write('body { font-family: sans-serif; text-align: center; margin: 20px; }');
          printWindow.document.write('svg { max-width: 300px; height: auto; display: block; margin: 0 auto; }');
          printWindow.document.write('</style>');
          printWindow.document.write('</head><body>');
          printWindow.document.write('<h2>Barcode Produk</h2>');
          printWindow.document.write(barcodeSvgElement.outerHTML);
          printWindow.document.write('</body></html>');
          printWindow.document.close();
          printWindow.focus();
          printWindow.print();
        }
      ", ns("barcode_svg_display")))
    })
    
    # --- BARU: Download dan Print ZPL untuk Barcode Assembler ---
    
    # Download handler untuk file ZPL
    output$download_barcode_zpl <- downloadHandler(
      filename = function() {
        req(rv$barcode_content)
        paste0("barcode_zpl_", rv$barcode_content, ".txt")
      },
      content = function(file) {
        req(rv$barcode_zpl)
        writeLines(rv$barcode_zpl, file)
      }
    )
    
    # observeEvent untuk tombol cetak ZPL
    observeEvent(input$print_barcode_zpl, {
      req(rv$barcode_zpl, rv$barcode_content)
      showModal(modalDialog(
        title = "Cetak Label Barcode ZPL",
        HTML(paste0(
          "<p>Untuk mencetak label fisik dengan printer Zebra:</p>",
          "<ol>",
          "<li>Klik tombol <strong>'Unduh ZPL'</strong> di bawah ini untuk mengunduh file ZPL.</li>",
          "<li>Gunakan software seperti <strong>Zebra Setup Utilities</strong> untuk mengirim file ZPL ini ke printer Anda.</li>",
          "</ol>",
          "<p><strong>Isi ZPL untuk No Produksi ", rv$barcode_content, ":</strong></p>",
          "<pre style='background-color:#f0f0f0; padding:10px; border-radius:5px; overflow-x:auto;'>",
          htmltools::htmlEscape(rv$barcode_zpl),
          "</pre>"
        )),
        easyClose = TRUE,
        footer = tagList(
          # Gunakan ns() di dalam modal juga untuk memastikan ID unik
          downloadButton(ns("download_barcode_zpl"), "Unduh ZPL", class = "btn-primary")
        )
      ))
    }, ignoreInit = TRUE)
    
    # --- Data Submission Logic for Tester and Packager ---
    
    # Reactive expression for items available for testing (not yet tested)
    available_for_testing <- reactive({
      all_assembler_nos <- reactive_data$assembler$no_produksi
      already_tested_nos <- reactive_data$tester$no_produksi
      not_yet_tested_nos <- all_assembler_nos[!all_assembler_nos %in% already_tested_nos]
      print(paste("Available for testing:", paste(not_yet_tested_nos, collapse = ", "))) # Debug print
      return(not_yet_tested_nos)
    })
    
    # ObserveEvent for submit_tester:
    observeEvent(input$test_no, {
      # Update test_no choices when reactive_data$assembler or reactive_data$tester changes
      updateSelectInput(session, "test_no", choices = available_for_testing())
    }, ignoreNULL = FALSE, ignoreInit = TRUE) # Update on change, but not on initial load
    
    observeEvent(input$submit_tester, {
      print("Submit Tester button clicked!") # Debug print
      req(input$test_no, input$test_date, input$test_pic, input$status)
      
      if (input$test_no %in% reactive_data$tester$no_produksi) {
        showModal(modalDialog("No Produksi ini sudah di-test. Mohon pilih yang lain atau update data yang sudah ada.", easyClose = TRUE))
        return()
      }
      
      new_entry <- data.frame(
        no_produksi = input$test_no,
        tanggal_testing = input$test_date,
        PIC = input$test_pic,
        status = input$status
      )
      
      print("Attempting to append new tester entry to DB.") # Debug print
      DBI::dbAppendTable(conn, "tester", new_entry)
      print("New tester entry appended to DB.") # Debug print
      
      reactive_data$tester <- read_and_convert_table(conn, "tester", c("tanggal_testing"))
      print(paste("Reactive data tester rows after update:", nrow(reactive_data$tester))) # Debug print
      
      # Clear inputs after submission and update choices *before* showing the modal
      updateSelectInput(session, "test_no", choices = available_for_testing(), selected = character(0))
      updateDateInput(session, "test_date", value = Sys.Date())
      updateTextInput(session, "test_pic", value = "")
      updateSelectInput(session, "status", selected = "Lolos")
      
      shinyjs::delay(200, {
        showModal(modalDialog("Data Tester berhasil disimpan!", easyClose = TRUE))
      })
    })
    
    # Reactive expression for items available for packaging
    available_for_packaging <- reactive({
      passed_testing <- reactive_data$tester %>%
        # PERBAIKAN: Gunakan dplyr::select secara eksplisit
        dplyr::select(no_produksi, tanggal_testing, PIC_testing = PIC) 
      
      packaged_items_list <- reactive_data$packager$items_in_package
      
      # Mengurai string items_in_package menjadi vektor no_produksi yang sudah dikemas
      already_packaged_nos <- character(0)
      if (length(packaged_items_list) > 0) {
        # Menggunakan strsplit dengan unlist dan trimws untuk menangani spasi ekstra
        already_packaged_nos <- unlist(lapply(packaged_items_list, function(x) trimws(strsplit(x, ",")[[1]])))
      }
      
      available_items <- passed_testing %>%
        filter(!no_produksi %in% already_packaged_nos)
      print(paste("Available for packaging:", paste(available_items$no_produksi, collapse = ", "))) # Debug print
      return(available_items)
    })
    
    # Observe changes in available_for_packaging to update pickerInput choices
    observe({
      # Dapatkan semua item yang tersedia untuk pengemasan (belum dikemas secara permanen)
      all_available_for_packaging <- available_for_packaging()$no_produksi
      
      # Dapatkan item yang saat ini dipilih oleh pengguna di pickerInput
      current_selection <- input$pack_nos 
      
      # Perbarui pickerInput.
      # 'choices' harus berisi semua item yang benar-benar tersedia untuk pengemasan.
      # 'selected' akan memberitahu pickerInput mana dari pilihan ini yang saat ini aktif.
      # pickerInput akan secara visual menangani tampilan item yang dipilih (misalnya, dengan tanda centang)
      # dan memungkinkan pembatalan pilihan.
      updatePickerInput(session, "pack_nos", 
                        choices = all_available_for_packaging, 
                        selected = current_selection)
    })
    
    observeEvent(input$submit_package, {
      print("Submit Package button clicked!") # Debug print
      req(input$pack_nos, input$pack_date, input$pack_pic)
      
      num_selected_items <- length(input$pack_nos)
      
      if (!(num_selected_items == 5 || num_selected_items == 10)) {
        showModal(modalDialog("Mohon pilih tepat 5 atau 10 barang untuk dikemas dalam satu paket.", easyClose = TRUE))
        return()
      }
      
      # --- START: FINAL PACKAGE NUMBERING FIX ---
      
      # Helper function to process a single package.
      process_package <- function(selected_nos_subset, generated_no_package, current_pic, current_pack_date, current_conn) {
        items_in_package_string <- paste(sort(selected_nos_subset), collapse = ",")
        
        new_entry <- data.frame(
          no_package = generated_no_package,
          tanggal_packaging = current_pack_date,
          PIC = current_pic,
          items_in_package = items_in_package_string,
          stringsAsFactors = FALSE
        )
        
        DBI::dbAppendTable(current_conn, "packager", new_entry)
        
        qrcode_data_for_package <- paste0(
          "Kode Paket: ", generated_no_package, "\n",
          "Tanggal Packaging: ", format(current_pack_date, "%d/%m/%Y"), "\n",
          "Items: ", items_in_package_string
        )
        
        zpl_string <- sprintf(
          "^XA\n^FO40,35^BQN,2,6^FDMA,%s^FS\n^FO300,60^A0N,35,35^FDPackage: %s^FS\n^FO300,110^A0N,28,28^FB500,5,0,L,0^FDContains: %s^FS\n^XZ",
          qrcode_data_for_package, 
          generated_no_package,
          items_in_package_string
        )
        
        return(list(message = paste0("Paket '", generated_no_package, "' berhasil dibuat."),
                    qrcode_data = qrcode_data_for_package,
                    package_no = generated_no_package,
                    zpl_string = zpl_string))
      }
      
      # Step 1: Determine the next sequence number based on all packages from the CURRENT MONTH.
      current_year_month <- format(Sys.Date(), "%Y%m") # e.g., "202508"
      
      # Filter for all packages within this month by parsing the YYYYMM part of the 'no_package' string.
      # In "PKG-YYYYMMDD-SSS", the YYYYMM part is at positions 5 through 10.
      existing_packages_this_month <- reactive_data$packager %>%
        filter(substr(no_package, 5, 10) == current_year_month)
      
      # Calculate the next sequence number based on this month's packages.
      next_package_sequence <- if (nrow(existing_packages_this_month) == 0) {
        1
      } else {
        # Extract the sequence part (last 3 characters) and find the maximum value.
        max_seq <- existing_packages_this_month$no_package %>%
          substr(15, 17) %>% # The sequence "SSS" is at positions 15, 16, 17.
          as.numeric() %>%
          max(na.rm = TRUE)
        max_seq + 1
      }
      
      # Get today's full date to use in the package ID format.
      current_date_package <- format(Sys.Date(), "%Y%m%d") # e.g., "20250808"
      
      # Step 2: Initialize variables for UI feedback
      rv$qrcode_info <- list() 
      results_list <- list()
      
      # Step 3: Process the selected items into one or two packages
      if (num_selected_items == 5) {
        # Construct the package number with the PKG-YYYYMMDD-SSS format and the new monthly sequence.
        generated_no_package <- paste0("PKG-", current_date_package, "-", sprintf("%03d", next_package_sequence))
        result <- process_package(input$pack_nos, generated_no_package, input$pack_pic, input$pack_date, conn)
        results_list <- list(result)
        
      } else if (num_selected_items == 10) {
        sorted_pack_nos <- sort(input$pack_nos)
        package1_nos <- sorted_pack_nos[1:5]
        package2_nos <- sorted_pack_nos[6:10]
        
        # Create the first package number
        package1_no <- paste0("PKG-", current_date_package, "-", sprintf("%03d", next_package_sequence))
        result1 <- process_package(package1_nos, package1_no, input$pack_pic, input$pack_date, conn)
        
        # Create the second package number by incrementing the sequence
        package2_no <- paste0("PKG-", current_date_package, "-", sprintf("%03d", next_package_sequence + 1))
        result2 <- process_package(package2_nos, package2_no, input$pack_pic, input$pack_date, conn)
        
        results_list <- list(result1, result2)
      }
      
      # Step 4: Aggregate results to show in the UI
      message_text_final <- paste(sapply(results_list, `[[`, "message"), collapse = "\n")
      rv$qrcode_info <- lapply(results_list, function(res) {
        list(data = res$qrcode_data, package_no = res$package_no, zpl_string = res$zpl_string)
      })
      
      # Step 5: Refresh the main reactive data from the database
      reactive_data$packager <- read_and_convert_table(conn, "packager", c("tanggal_packaging"))
      
      # Step 6: Clear UI inputs and show the final confirmation modal
      updatePickerInput(session, "pack_nos", choices = available_for_packaging()$no_produksi, selected = character(0))
      updateDateInput(session, "pack_date", value = Sys.Date())
      updateTextInput(session, "pack_pic", value = "")
      
      shinyjs::delay(200, {
        showModal(modalDialog(HTML(gsub("\n", "<br>", message_text_final)), easyClose = TRUE))
      })
      
      # --- END: FINAL PACKAGE NUMBERING FIX ---
    })
    
    # --- Dynamic QR Code Display and Download for Packager ---
    output$qrcode_output_area <- renderUI({
      req(length(rv$qrcode_info) > 0) # Pastikan ada data QR code
      
      qr_output_list <- lapply(seq_along(rv$qrcode_info), function(i) {
        qr_data <- rv$qrcode_info[[i]]$data
        qr_package_no <- rv$qrcode_info[[i]]$package_no
        zpl_string_for_download <- rv$qrcode_info[[i]]$zpl_string # Ambil ZPL string
        
        # Buat outputId unik untuk setiap gambar QR code
        image_id <- paste0("qr_image_", i)
        download_png_id <- paste0("download_qr_png_", i) # ID baru untuk download PNG
        download_zpl_id <- paste0("download_qr_zpl_", i) # ID baru untuk download ZPL
        print_id <- paste0("print_qr_", i)
        
        # Definisikan renderImage untuk setiap QR code secara dinamis
        output[[image_id]] <- renderImage({
          outfile <- tempfile(fileext = ".png")
          qr_matrix <- qrcode::qr_code(qr_data)
          png(outfile, width = 200, height = 200, units = "px", res = 72) # Ukuran lebih kecil untuk tampilan
          par(mar = c(0, 0, 0, 0)) # Hapus margin plot
          image(qr_matrix, col = c("white", "black"), asp = 1, axes = FALSE)
          dev.off()
          
          list(src = outfile,
               contentType = "image/png",
               width = 200,
               height = 200,
               alt = paste("QR Code Paket", qr_package_no))
        }, deleteFile = TRUE)
        
        # Definisikan downloadHandler untuk setiap QR code (PNG) secara dinamis
        output[[download_png_id]] <- downloadHandler(
          filename = function() {
            paste0("qrcode_", qr_package_no, ".png") # Nama file unik berdasarkan kode paket
          },
          content = function(file) {
            qr_matrix <- qrcode::qr_code(qr_data)
            png(file, width = 300, height = 300, units = "px", res = 72)
            par(mar = c(0, 0, 0, 0))
            image(qr_matrix, col = c("white", "black"), asp = 1, axes = FALSE)
            dev.off()
          }
        )
        
        # Definisikan downloadHandler untuk ZPL secara dinamis
        output[[download_zpl_id]] <- downloadHandler(
          filename = function() {
            paste0("label_zpl_", qr_package_no, ".txt") # Nama file ZPL
          },
          content = function(file) {
            writeLines(zpl_string_for_download, file) # Tulis string ZPL ke file
          }
        )
        
        # Definisikan observeEvent untuk tombol cetak setiap QR code secara dinamis
        observeEvent(input[[print_id]], {
          req(qr_data) # Pastikan data QR code tersedia
          showModal(modalDialog(
            title = "Cetak Label ZPL",
            HTML(paste0(
              "<p>Untuk mencetak label fisik dengan printer Zebra:</p>",
              "<ol>",
              "<li>Klik tombol <strong>'Unduh ZPL'</strong> di bawah ini untuk mengunduh file ZPL.</li>",
              "<li>Buka <strong>Zebra Setup Utilities</strong> (atau aplikasi pengiriman ZPL lainnya) di komputer yang terhubung ke printer Zebra Anda.</li>",
              "<li>Muat atau tempelkan isi file ZPL yang diunduh ke aplikasi tersebut dan kirimkan ke printer.</li>",
              "</ol>",
              "<p><strong>Isi ZPL untuk Paket ", qr_package_no, ":</strong></p>",
              "<pre style='background-color:#f0f0f0; padding:10px; border-radius:5px; overflow-x:auto;'>",
              htmltools::htmlEscape(zpl_string_for_download), # Tampilkan ZPL string
              "</pre>"
            )),
            easyClose = TRUE,
            footer = tagList(
              downloadButton(ns(download_zpl_id), paste("Unduh ZPL untuk", qr_package_no), class = "btn-primary") # Tambahkan tombol unduh ZPL di modal
            )
          ))
        }, ignoreInit = TRUE, once = FALSE)
        
        # Kembalikan elemen UI untuk setiap QR code
        fluidRow(
          box(
            title = paste("QR Code Paket", qr_package_no), status = "info", solidHeader = TRUE, width = 12,
            imageOutput(ns(image_id)), # Tampilkan gambar QR code
            fluidRow(
              column(4, downloadButton(ns(download_png_id), paste("Unduh PNG", qr_package_no), style = "width:100%; margin-top: 10px;")),
              column(4, downloadButton(ns(download_zpl_id), paste("Unduh ZPL", qr_package_no), style = "width:100%; margin-top: 10px;")), # Tombol unduh ZPL
              column(4, actionButton(ns(print_id), paste("Cetak ZPL", qr_package_no), class = "btn-info", style = "width:100%; margin-top: 10px;"))
            )
          )
        )
      })
      
      do.call(tagList, qr_output_list) # Gabungkan semua elemen UI menjadi satu tagList
    })
    
    # --- Data Table Outputs (for regular users) ---
    
    output$assembler_table <- DT::renderDataTable({
      # Debugging: print data being sent to table
      print("Rendering assembler table. Data head:")
      print(head(reactive_data$assembler))
      reactive_data$assembler
    }, options = list(pageLength = 5, scrollX = TRUE))
    
    output$tester_table <- DT::renderDataTable({
      # Menampilkan data assembler yang belum di-test untuk tester
      reactive_data$assembler %>%
        filter(!no_produksi %in% reactive_data$tester$no_produksi)
    }, options = list(pageLength = 5, scrollX = TRUE))
    
    output$packager_available_table <- DT::renderDataTable({
      available_for_packaging()
    }, options = list(pageLength = 5, scrollX = TRUE))
    
    output$packages_created_table <- DT::renderDataTable({
      reactive_data$packager %>%
        mutate(
          tanggal_packaging = format(tanggal_packaging, "%d/%m/%Y")
        )
    }, options = list(pageLength = 5, scrollX = TRUE))
  })
}
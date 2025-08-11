# modules/admin_debug_server.R

admin_debug_server <- function(id, reactive_data, pool) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Event for creating 100 assembler units
    observeEvent(input$debug_create_units, {
      showModal(modalDialog(
        title = "Confirm Data Generation",
        "Are you sure you want to generate 100 new assembler units?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_create_units"), "Yes, Create")
        )
      ))
    })
    
    observeEvent(input$confirm_create_units, {
      removeModal()
      withProgress(message = 'Generating Assembler Data...', value = 0, {
        
        # Get the latest sequence number for today's month/year
        query <- "SELECT MAX(CAST(SUBSTRING(no_produksi, 1, 4) AS INTEGER)) FROM assembler WHERE SUBSTRING(no_produksi, 5, 4) = $1;"
        current_month_year_suffix <- format(Sys.Date(), "%m%y")
        max_seq_result <- DBI::dbGetQuery(pool, query, params = list(current_month_year_suffix))
        start_sequence <- if (is.na(max_seq_result[[1]])) 1 else max_seq_result[[1]] + 1
        
        new_units <- 100
        assembler_data <- vector("list", new_units)
        
        for (i in 1:new_units) {
          incProgress(1/new_units, detail = paste("Creating unit", i))
          
          seq_num <- start_sequence + i - 1
          no_produksi <- paste0(sprintf("%04d", seq_num), current_month_year_suffix)
          
          start_date <- Sys.Date() - sample(5:20, 1)
          stop_date <- start_date + sample(1:4, 1)
          
          assembler_data[[i]] <- list(
            no_produksi = no_produksi,
            tanggal_start = start_date,
            tanggal_stop = stop_date,
            PIC = paste0("Assembler-", sample(1:5, 1))
          )
        }
        
        # Convert list to data frame and write to DB
        df <- do.call(rbind.data.frame, assembler_data)
        DBI::dbAppendTable(pool, "assembler", df)
        
        # Refresh reactive data
        reactive_data$assembler <- read_and_convert_table(pool, "assembler", c("tanggal_start", "tanggal_stop"))
        toastr_success(paste(new_units, "assembler units created successfully!"))
      })
    })
    
    # Event for testing units
    observeEvent(input$debug_test_units, {
      # Find units that are in assembler but not in tester table
      available_to_test <- reactive_data$assembler %>%
        filter(!no_produksi %in% reactive_data$tester$no_produksi)
      
      if(nrow(available_to_test) == 0) {
        toastr_info("No new units available to test.")
        return()
      }
      
      withProgress(message = 'Generating Test Data...', value = 0, {
        
        num_to_test <- nrow(available_to_test)
        tester_data <- vector("list", num_to_test)
        
        for(i in 1:num_to_test) {
          incProgress(1/num_to_test, detail = paste("Testing unit", i))
          
          unit <- available_to_test[i,]
          
          tester_data[[i]] <- list(
            no_produksi = unit$no_produksi,
            tanggal_testing = unit$tanggal_stop + sample(1:3, 1),
            PIC = paste0("Tester-", sample(1:3, 1)),
            status = sample(c("Lolos", "Tidak Lolos"), 1, prob = c(0.9, 0.1)) # 90% pass rate
          )
        }
        
        df <- do.call(rbind.data.frame, tester_data)
        DBI::dbAppendTable(pool, "tester", df)
        
        reactive_data$tester <- read_and_convert_table(pool, "tester", c("tanggal_testing"))
        toastr_success(paste(num_to_test, "units tested successfully!"))
      })
    })
    
    # Event for packaging units
    observeEvent(input$debug_pack_units, {
      passed_units <- reactive_data$tester %>% filter(status == "Lolos")
      
      packaged_items <- unlist(lapply(reactive_data$packager$items_in_package, function(x) trimws(strsplit(x, ",")[[1]])))
      
      available_to_pack <- passed_units %>% filter(!no_produksi %in% packaged_items)
      
      if(nrow(available_to_pack) < 5) {
        toastr_info("Not enough passed units to create a new package (minimum 5).")
        return()
      }
      
      withProgress(message = 'Generating Packages...', value = 0, {
        
        # Get next package sequence number
        current_date_package <- format(Sys.Date(), "%Y%m%d")
        query <- "SELECT MAX(CAST(SUBSTRING(no_package, 15, 3) AS INTEGER)) FROM packager WHERE SUBSTRING(no_package, 5, 8) = $1;"
        max_seq_result <- DBI::dbGetQuery(pool, query, params = list(current_date_package))
        start_sequence <- if (is.na(max_seq_result[[1]])) 1 else max_seq_result[[1]] + 1
        
        # Split available units into chunks of 5
        item_chunks <- split(available_to_pack, ceiling(seq_along(available_to_pack$no_produksi)/5))
        
        num_packages <- length(item_chunks)
        packager_data <- vector("list", num_packages)
        
        for(i in 1:num_packages) {
          incProgress(1/num_packages, detail = paste("Creating package", i))
          
          chunk <- item_chunks[[i]]
          
          # Skip if chunk is less than 5 (last chunk might be smaller)
          if(nrow(chunk) < 5) next
          
          seq_num <- start_sequence + i - 1
          
          packager_data[[i]] <- list(
            no_package = paste0("PKG-", current_date_package, "-", sprintf("%03d", seq_num)),
            tanggal_packaging = max(chunk$tanggal_testing) + 1,
            PIC = paste0("Packager-", sample(1:2, 1)),
            items_in_package = paste(sort(chunk$no_produksi), collapse = ",")
          )
        }
        
        df <- do.call(rbind.data.frame, compact(packager_data)) # compact removes NULLs
        DBI::dbAppendTable(pool, "packager", df)
        
        reactive_data$packager <- read_and_convert_table(pool, "packager", c("tanggal_packaging"))
        toastr_success(paste(nrow(df), "packages created successfully!"))
      })
    })
    
    # Event for clearing all data
    observeEvent(input$debug_clear_all_data, {
      showModal(modalDialog(
        title = "CONFIRM: DELETE ALL DATA",
        "This is irreversible. Are you absolutely sure you want to delete all production data from the assembler, tester, and packager tables?",
        easyClose = TRUE,
        footer = tagList(
          modalButton("CANCEL"),
          actionButton(ns("confirm_clear_data"), "YES, DELETE EVERYTHING", class = "btn-danger")
        )
      ))
    })
    
    observeEvent(input$confirm_clear_data, {
      removeModal()
      tryCatch({
        DBI::dbExecute(pool, "TRUNCATE TABLE packager, tester, assembler;")
        reactive_data$assembler <- read_and_convert_table(pool, "assembler", c("tanggal_start", "tanggal_stop"))
        reactive_data$tester <- read_and_convert_table(pool, "tester", c("tanggal_testing"))
        reactive_data$packager <- read_and_convert_table(pool, "packager", c("tanggal_packaging"))
        toastr_warning("All production data has been cleared.", position = "top-center")
      }, error = function(e) {
        toastr_error(paste("Error clearing data:", e$message))
      })
    })
    
  })
}

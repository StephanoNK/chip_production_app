# modules/data_entry_ui.R

# Fungsi ini sekarang menerima 'user_role' dan 'ns' sebagai argumen
data_entry_ui <- function(user_role, ns) { 
  
  tabItem(tabName = "entry",
          h2(paste("Input Data untuk", user_role())),
          textOutput(ns("debug_output")), # Debug output
          if (user_role() == "Assembler") {
            fluidPage(
              box(
                title = "Data Assembler", status = "primary", solidHeader = TRUE, width = 12,
                DT::dataTableOutput(ns("assembler_table")) 
              ),
              box(
                title = "Input Data Assembler Baru", status = "warning", solidHeader = TRUE, width = 6,
                fluidRow(
                  column(6, actionButton(ns("start_assembly"), "Start Assembly", class = "btn-success", width = '100%')),
                  column(6, actionButton(ns("stop_assembly"), "Stop Assembly", class = "btn-danger", width = '100%'))
                ),
                br(),
                textOutput(ns("assembly_status")),
                br(),
                textInput(ns("pic"), "PIC (Person In Charge)"),
                helpText("Kode Produksi (4 digit urutan + 2 digit bulan + 2 digit tahun) akan otomatis dibuat saat 'Stop Assembly' ditekan.")
              ),
              box( # Box untuk Barcode telah diperbarui
                title = "Barcode Produk yang Baru Dibuat", status = "info", solidHeader = TRUE, width = 6,
                helpText(HTML("Barcode (visual) akan muncul di bawah. Gunakan tombol untuk mengunduh format SVG atau ZPL untuk pencetakan label.")),
                tags$svg(id = ns("barcode_svg_display"), style = "width: 100%; height: 100px;"), # SVG element for JsBarcode
                fluidRow(
                  column(6, actionButton(ns("download_barcode_js"), "Unduh SVG", style = "width:100%; margin-top: 10px;")),
                  column(6, downloadButton(ns("download_barcode_zpl"), "Unduh ZPL", style = "width:100%; margin-top: 10px;")) # Tombol Download ZPL Baru
                ),
                fluidRow(
                  column(6, actionButton(ns("print_barcode"), "Cetak Visual", class = "btn-info", style = "width:100%; margin-top: 10px;")),
                  column(6, actionButton(ns("print_barcode_zpl"), "Cetak ZPL", class = "btn-info", style = "width:100%; margin-top: 10px;")) # Tombol Cetak ZPL Baru
                )
              )
            )
          } else if (user_role() == "Tester") {
            fluidPage(
              box(
                title = "Data Assembler (untuk Testing)", status = "primary", solidHeader = TRUE, width = 12,
                DT::dataTableOutput(ns("tester_table"))
              ),
              box(
                title = "Input Data Tester Baru", status = "warning", solidHeader = TRUE, width = 6,
                selectInput(ns("test_no"), "Pilih No Produksi", choices = NULL), # Choices set in server
                dateInput(ns("test_date"), "Tanggal Testing", value = Sys.Date(), format = "dd/mm/yyyy"),
                textInput(ns("test_pic"), "PIC (Person In Charge)"),
                selectInput(ns("status"), "Status", choices = c("Lolos", "Tidak Lolos")),
                actionButton(ns("submit_tester"), "Submit Data Tester", class = "btn-success")
              )
            )
          } else if (user_role() == "Packager") {
            fluidPage(
              box(
                title = "Barang Tersedia untuk Pengemasan (Lolos Testing)", status = "primary", solidHeader = TRUE, width = 12,
                DT::dataTableOutput(ns("packager_available_table"))
              ),
              box(
                title = "Input Data Packager Baru", status = "warning", solidHeader = TRUE, width = 6,
                # Mengganti selectInput dengan pickerInput
                pickerInput(
                  inputId = ns("pack_nos"),
                  label = "Pilih No Produksi (Pilih 5 atau 10 Barang)",
                  choices = NULL, # Choices akan diatur di server
                  multiple = TRUE,
                  options = pickerOptions(
                    actionsBox = TRUE, # Menampilkan tombol Select All/Deselect All
                    liveSearch = TRUE, # Mengaktifkan fitur pencarian
                    liveSearchPlaceholder = "Cari No Produksi...",
                    selectedTextFormat = "count > 3", # Menampilkan ringkasan jika lebih dari 3 item dipilih
                    noneSelectedText = "Tidak ada barang yang dipilih"
                  )
                ), 
                dateInput(ns("pack_date"), "Tanggal Packaging", value = Sys.Date(), format = "dd/mm/yyyy"),
                textInput(ns("pack_pic"), "PIC (Person In Charge)"),
                actionButton(ns("submit_package"), "Submit Package", class = "btn-success")
              ),
              # Menggunakan uiOutput untuk menampilkan QR Code secara dinamis
              uiOutput(ns("qrcode_output_area")), 
              box(
                title = "Riwayat Paket yang Dibuat", status = "info", solidHeader = TRUE, width = 12,
                DT::dataTableOutput(ns("packages_created_table"))
              )
            )
          }
  )
}
# modules/admin_barang_ui.R

admin_barang_ui <- function(ns) {
  tabItem(tabName = "admin_barang",
          h2("Admin: Kelola Data Barang"),
          tabBox(
            title = "Manajemen Data Produksi",
            id = ns("tabset_admin_barang"),
            height = "600px", width = 12,
            tabPanel("Assembler",
                     fluidRow(
                       box(title = "Tambah Data Assembler Baru", status = "warning", solidHeader = TRUE, width = 12,
                           textInput(ns("admin_assembler_no_produksi"), "No Produksi (Wajib, unik)"),
                           dateInput(ns("admin_assembler_tanggal_start"), "Tanggal Mulai", value = Sys.Date(), format = "dd/mm/yyyy"),
                           dateInput(ns("admin_assembler_tanggal_stop"), "Tanggal Selesai", value = Sys.Date(), format = "dd/mm/yyyy"),
                           textInput(ns("admin_assembler_pic"), "PIC"),
                           actionButton(ns("admin_add_assembler"), "Tambah Data Assembler", class = "btn-success")
                       ),
                       box(title = "Data Assembler (Edit Langsung)", status = "primary", solidHeader = TRUE, width = 12,
                           helpText("Klik dua kali pada sel untuk mengedit. Tekan Enter untuk menyimpan. Pilih satu baris untuk opsi lainnya."),
                           DT::dataTableOutput(ns("admin_assembler_crud_table")),
                           # Placeholder for reprint button
                           uiOutput(ns("assembler_reprint_ui")),
                           actionButton(ns("admin_delete_assembler"), "Hapus Data Terpilih", class = "btn-danger", style = "margin-top: 10px;")
                       )
                     )
            ),
            tabPanel("Tester",
                     fluidRow(
                       box(title = "Tambah Data Tester Baru", status = "warning", solidHeader = TRUE, width = 12,
                           textInput(ns("admin_tester_no_produksi"), "No Produksi (Wajib, unik)"),
                           dateInput(ns("admin_tester_tanggal_testing"), "Tanggal Testing", value = Sys.Date(), format = "dd/mm/yyyy"),
                           textInput(ns("admin_tester_pic"), "PIC"),
                           selectInput(ns("admin_tester_status"), "Status", choices = c("Lolos", "Tidak Lolos")),
                           actionButton(ns("admin_add_tester"), "Tambah Data Tester", class = "btn-success")
                       ),
                       box(title = "Data Tester (Edit Langsung)", status = "primary", solidHeader = TRUE, width = 12,
                           helpText("Klik dua kali pada sel untuk mengedit. Tekan Enter untuk menyimpan."),
                           DT::dataTableOutput(ns("admin_tester_crud_table")),
                           actionButton(ns("admin_delete_tester"), "Hapus Data Terpilih", class = "btn-danger", style = "margin-top: 10px;")
                       )
                     )
            ),
            tabPanel("Packager",
                     fluidRow(
                       box(title = "Tambah Data Packager Baru", status = "warning", solidHeader = TRUE, width = 12,
                           textInput(ns("admin_packager_no_package"), "No Paket (Wajib, unik)"),
                           dateInput(ns("admin_packager_tanggal_packaging"), "Tanggal Packaging", value = Sys.Date(), format = "dd/mm/yyyy"),
                           textInput(ns("admin_packager_pic"), "PIC"),
                           textInput(ns("admin_packager_items_in_package"), "Item dalam Paket (pisahkan dengan koma)"),
                           actionButton(ns("admin_add_packager"), "Tambah Data Packager", class = "btn-success")
                       ),
                       box(title = "Data Packager (Edit Langsung)", status = "primary", solidHeader = TRUE, width = 12,
                           helpText("Klik dua kali pada sel untuk mengedit. Tekan Enter untuk menyimpan. Pilih satu baris untuk opsi lainnya."),
                           DT::dataTableOutput(ns("admin_packager_crud_table")),
                           # Placeholder for reprint button
                           uiOutput(ns("packager_reprint_ui")),
                           actionButton(ns("admin_delete_packager"), "Hapus Data Terpilih", class = "btn-danger", style = "margin-top: 10px;")
                       )
                     )
            )
          )
  )
}

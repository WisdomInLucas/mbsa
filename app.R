#load package

library(shiny)
library(shinydashboard)	
library(shinyWidgets)
library(tidyverse)
library(randomForest)
library(scales)
library(fontawesome)
library(shinyjs) 


# define data
features_for_ml <- c('income','expense','debt','saving',
                     'shoppingFreq','investment','investmentType','investmentRisk',
                     'payment','savingRate','shoppingCat','insurance'
                     ,'recordExpense','priority')


dummy_data_path <- "financial_behaviour.csv"
survey_responses_file <- "survey_responses_data.csv"

financial_ml_model <- NULL
df_ml_train_global_levels <- NULL
df_ml_train_initial <- NULL

tryCatch({
  if (!file.exists(dummy_data_path)) {
    stop(paste0("ERROR: File dummy data '", dummy_data_path, "' tidak ditemukan. \nHarap pastikan file ini (dengan nama dan ekstensi yang sama persis) \nberada di direktori yang sama dengan file app.R Anda."))
  }
  df_ml_train_initial <- read_csv(dummy_data_path, show_col_types = FALSE)
  df_ml_train_initial$classification <- as.factor(df_ml_train_initial$classification)
  
  df_ml_train_global_levels <- lapply(df_ml_train_initial, levels)
  if (!all(features_for_ml %in% names(df_ml_train_initial))) {
    missing_features <- setdiff(features_for_ml, names(df_ml_train_initial))
    if(length(missing_features) > 0) {
      warning(paste0("PERINGATAN: Fitur baru ditambahkan ke survei tetapi tidak ada di data training ML (", dummy_data_path, "): ", paste(missing_features, collapse = ", "),
                     "\nFitur ini TIDAK akan digunakan oleh model ML untuk klasifikasi hingga data training diperbarui."))
      features_for_ml_for_training <- intersect(features_for_ml, names(df_ml_train_initial))
    } else {
      features_for_ml_for_training <- features_for_ml
    }
  } else {
    features_for_ml_for_training <- features_for_ml
  }
  
  set.seed(123)
  financial_ml_model <- randomForest(
    as.formula(paste("classification ~", paste(features_for_ml_for_training, collapse = " + "))),
    data = df_ml_train_initial, 
    ntree = 500,
    importance = TRUE,
    na.action = na.omit
  )
  message("Model Random Forest berhasil dilatih saat startup aplikasi.")
}, error = function(e) {
  stop(paste0("ERROR saat menyiapkan model ML: ", e$message,"\nMohon periksa kembali struktur data di '", dummy_data_path, "' atau konfigurasi model ML."))
})

# css
custom_css <- paste0(
  "body { font-family: 'Open Sans', sans-serif; background-color: #f8f9fa; color: #343a40; }",
  ".navbar-brand { font-weight: bold; font-size: 24px; color: #0056b3 !important; }",
  ".navbar-default .navbar-nav > li > a { font-weight: 500; color: #343a40; }",
  ".navbar-default .navbar-nav > .active > a, .navbar-default .navbar-nav > .active > a:hover, .navbar-default .navbar-nav > .active > a:focus { background-color: #e9ecef; color: #343a40; border-bottom: 2px solid #343a40; }",
  ".navbar-default { background-color: #ffffff; border-color: #e9ecef; box-shadow: 0 2px 5px rgba(0,0,0,.05); }",
  
  ".box { border-radius: 8px; box-shadow: 0 4px 8px rgba(0,0,0,.05); margin-bottom: 25px; border: 1px solid #e9ecef; }",
  ".box-header.with-border { border-bottom: 1px solid #e9ecef; padding: 15px; background-color: #ffffff; color: #343a40; }",
  ".box-title { font-size: 24px; font-weight: 600; color: #343a40; }",
  ".box-body { padding: 20px; background-color: #ffffff; }",
  
  ".box.box-solid.box-primary > .box-header { background-color: #ffffff; color: #343a40; border-bottom: 1px solid #e9ecef; }",
  ".box.box-solid.box-success > .box-header { background-color: #ffffff; color: #343a40; border-bottom: 1px solid #e9ecef; }",
  ".box.box-solid.box-info > .box-header { background-color: #ffffff; color: #343a40; border-bottom: 1px solid #e9ecef; }",
  
  "h1, h2, h3, h4, h5, h6 { font-weight: 600; color: #343a40; margin-bottom: 15px; }",
  "p { font-size: 16px; line-height: 1.7; margin-bottom: 10px; }",
  
  ".btn-primary, .btn-success, .btn-info { background-color: #e9ecef; border-color: #ced4da; color: #343a40; font-weight: bold; padding: 10px 20px; border-radius: 5px; }",
  ".btn-primary:hover, .btn-success:hover, .btn-info:hover { background-color: #ced4da; border-color: #adb5bd; color: #343a40; opacity: 1; }",
  
  ".small-box { border-radius: 8px; box-shadow: 0 5px 15px rgba(0,0,0,.1); background-color: #ffffff; color: #343a40; }",
  ".small-box h3, .small-box p { color: #343a40 !important; }",
  ".small-box .icon { top: -20px; }",
  
  ".shiny-input-container { margin-bottom: 20px; padding: 5px; border-radius: 5px; }",
  ".well { background-color: #ffffff; border: 1px solid #e9ecef; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,.04); padding: 25px; margin-bottom: 25px; }"
)




ui <- navbarPage(
  title = div(
    tags$img(
      src = "logo_mbsa.png",
      style = "height: 55px; margin-top: -10px; margin-right: 10px;"
    ),
    "MBSA Financial Dashboard"
  ),
  id = "main_navbar",
  
  tags$head(
    tags$style(HTML(custom_css)),
    
    # Tambahkan style opsional untuk navbar logo
    tags$style(HTML(".navbar-brand > img { margin-right: 10px; vertical-align: middle; }")),
    
    tags$link(href="https://fonts.googleapis.com/css2?family=Open+Sans:wght@300;400;500;600;700&display=swap", rel="stylesheet"),
    useShinyjs()
  ),
  
  
  tabPanel("Home", value = "home_tab", icon = icon("home"),
           fluidPage(
             column(12,
                    box(
                      title = "Selamat Datang di MBSA 🚀",
                      status = "primary", width = 12, align = "center",
                      
                      # Tambahkan logo di atas judul utama
                      tags$img(
                        src = "logo_mbsa.png",
                        style = "height: 120px; width: auto; margin-bottom: 20px;"
                      ),
                      h2("Analisis Finansialmu, Jadi Lebih Mudah!", style = "font-weight: 600;"),
                      p("MBSA adalah platform survei interaktif berbasis R Shiny yang dirancang untuk memetakan kondisi keuangan generasi muda Indonesia."),
                      p("Hasil survei ini akan membantu perusahaan perbankan dalam menentukan strategi pemasaran produk finansial yang sesuai dengan perilaku pasar terkini."),
                      br(),
                      actionButton("startSurveyHome", "Mulai Survey Sekarang", class = "btn-primary btn-lg")
                    ),
                    
                    box(
                      title = "Lebih Jauh Tentang MBSA",
                      status = "primary", width = 12,
                      
                      h3("📌 TENTANG MBSA"),
                      p("Market Bank Survey Analysis (MBSA) adalah platform survei interaktif berbasis R Shiny yang dirancang untuk memetakan kondisi keuangan generasi muda Indonesia. MBSA hadir untuk membantu pengguna memahami kondisi keuangan mereka secara personal melalui survei sederhana, namun menyentuh berbagai aspek penting dalam pengelolaan finansial."),
                      p("Melalui platform ini, MBSA juga menjadi sarana bagi perusahaan perbankan untuk mendapatkan data dan insight seputar perilaku keuangan masyarakat, khususnya generasi muda. Data tersebut dapat diolah menjadi strategi pemasaran produk finansial yang lebih tepat sasaran dan sesuai dengan kebutuhan pasar terkini."),
                      br(), 
                      
                      h3("📌 VISI MBSA"),
                      p("Menjadi platform analisis keuangan generasi muda yang adaptif, informatif, dan berbasis data aktual untuk mendukung terciptanya ekosistem finansial yang sehat, berkelanjutan, serta relevan dengan perilaku dan kebutuhan pasar di Indonesia.")
                    )
             )
           )
  ),
  tabPanel("Mulai Survei & Isi Data", value = "start_survey_tab", icon = icon("play-circle"),
           fluidPage(
             column(12,
                    box(
                      title = "Isi Data Finansial Kamu 📊",
                      status = "primary", width = 12,
                      p("Isilah pertanyaan-pertanyaan berikut dengan jujur untuk mendapatkan hasil analisis finansial yang akurat."),
                      br(),
                      fluidRow(
                        column(6,
                               wellPanel(
                                 h3("Informasi Keuangan Utama", class = "text-center"),
                                 numericInput("income", "1. Pendapatan Bulanan Bersih (Setelah Pajak) (Rp)", value = 0, min = 0, step = 100000),
                                 numericInput("expense", "2. Pengeluaran Bulanan Rata-rata (Rp)", value = 0, min = 0, step = 100000),
                                 numericInput("debt", "3. Total Cicilan Utang per Bulan (Rp)", value = 0, min = 0, step = 50000),
                                 numericInput("saving", "4. Total Tabungan cash (Rp)", value = 0, min = 0, step = 100000)
                               )
                        ),
                        column(6,
                               wellPanel(
                                 h3("Perilaku & Preferensi Finansial", class = "text-center"),
                                 radioButtons("shoppingFreq", "5. Seberapa sering anda melakukan self reward dalam sebulan?",
                                              choices = c("1 - Tidak Pernah",
                                                          "2 - Sesekali",
                                                          "3 - Sering")),
                                 radioButtons("investment", "6. Apakah Anda Saat Ini Memiliki Investasi?",
                                              choices = c("Ya", "Tidak"), selected = "Tidak"),
                                 conditionalPanel(
                                   condition = "input.investment == 'Ya'",
                                   checkboxGroupInput("investmentType", "Jenis Investasi yang Dimiliki (pilih semua yang sesuai):", choices = c("Saham", "Reksa Dana", "Emas/Logam Mulia", "Properti", "P2P Lending", "Crypto", "Lainnya"), selected = character(0)),
                                   sliderInput("investmentRisk", "Seberapa Berani Anda Mengambil Risiko dalam Berinvestasi? (1=Sangat Rendah, 5=Sangat Tinggi)", min = 1, max = 5, value = 3, step = 1),
                                   numericInput("investmentAmount", "6a. Berapa banyak yang Anda sisihkan untuk investasi (per bulan)? (Rp)", value = 0, min = 0, step = 10000)
                                   
                                   
                                 ),
                                 radioButtons("payment", "7. Metode Pembayaran Favorit Anda untuk Transaksi Sehari-hari:", choices = c("Cash", "Kartu Debit", "Kartu Kredit", "QRIS", "E-Wallet"), selected = "QRIS"),
                                 selectInput("shoppingCat", "8. Kategori Belanja (non-kebutuhan pokok) Favorit Anda:", choices = c("Makanan/Minuman (Kuliner/Coffee Shop)", "Fashion/Pakaian", "Gadget/Elektronik", "Hiburan (Film/Konser/Game)", "Travel/Liburan", "Hobi/Koleksi", "Lainnya"), selected = "Makanan/Minuman (Kuliner/Coffee Shop)"),
                                 checkboxGroupInput("insurance", "9. Jenis Asuransi yang Anda Miliki (pilih semua yang sesuai):", choices = c("Kesehatan (BPJS/Swasta)", "Jiwa", "Kendaraan", "Properti", "Pendidikan", "Tidak Ada"), selected = "Tidak Ada"),
                                 radioButtons("recordExpense", "10. Apakah Anda Rutin Mencatat atau Melacak Pengeluaran Anda?",
                                              choices = c("1 - Tidak Pernah",
                                                          "2 - Sesekali",
                                                          "3 - Sering"), selected = "2 - Sesekali "),
                                 selectInput("priority", "11. Apa Prioritas Finansial Utama Anda Saat Ini?", choices = c("Meningkatkan Tabungan Darurat", "Melunasi Utang", "Berinvestasi Jangka Panjang", "Meningkatkan Gaya Hidup", "Membeli Aset Besar (Rumah/Kendaraan)"), selected = "Meningkatkan Tabungan Darurat")
                               )
                        )
                      ),
                      tags$hr(),
                      actionButton("submitSurvey", "Kirim Survey", class = "btn-success btn-lg center-block")
                    )
             )
           )
  ),
  tabPanel("Tipe Keuangan", value = "wealth_types_tab", icon = icon("lightbulb"),
           fluidPage(
             column(12,
                    box(
                      title = "Tipe-tipe Keuangan Anak Muda 🔥",
                      status = "info", width = 12,
                      p("Hasil survey kamu nanti akan diklasifikasikan ke salah satu tipe berikut:"),
                      fluidRow(
                        column(3, align = "center",
                               tags$img(src = "sultan.png", height = "120px", style = "margin-bottom:10px;"),
                               h4("SULTAN"),
                               p("Pendapatan sangat tinggi, pengelolaan aset prima, dan investasi terdiversifikasi.")
                        ),
                        column(3, align = "center",
                               tags$img(src = "finance.png", height = "120px", style = "margin-bottom:10px;"),
                               h4("Man In Finance"),
                               p("Stabil, disiplin investasi, fokus growth aset jangka panjang.")
                        ),
                        column(3, align = "center",
                               tags$img(src = "crypto.png", height = "120px", style = "margin-bottom:10px;"),
                               h4("Crypto Bros"),
                               p("Pecinta crypto, high risk high gain, minim diversifikasi.")
                        ),
                        column(3, align = "center",
                               tags$img(src = "jaksel.png", height = "120px", style = "margin-bottom:10px;"),
                               h4("Jaksel Maksa"),
                               p("Gaya hidup mewah, income pas-pasan, tabungan tipis.")
                        )
                      ),
                      p(em("Catatan: Klasifikasi akhir akan menggunakan model Machine Learning dari data perilaku finansial."))
                    )
             )
           )
  ),
  tabPanel("Hasil", value = "results_tab", icon = icon("chart-line"),
           fluidPage(
             column(12, align = "center",
                    box(
                      title = "Hasil Klasifikasi Finansial Kamu 🔍",
                      status = "info", width = 12,
                      uiOutput("classificationIntro"),
                      h3(uiOutput("classificationResult"), style = "font-weight: bold; color: #007bff;"),
                      h4(textOutput("userClusterResult"), style = "font-weight: 500; color: #28a745;"),
                      uiOutput("resultRecommendation"),
                      br(),
                      actionButton("resetSurveyResult", "Isi Ulang Survey", class = "btn-primary")
                      
                    )
             )
           )
  ),
  tabPanel("Client Dashboard", value = "client_dashboard_tab", icon = icon("dashboard"),
           fluidPage(
             fluidRow(
               valueBoxOutput("totalRespondentsBox", width = 3),
               valueBoxOutput("topSegmentBox", width = 3),
               valueBoxOutput("avgIncomeBox", width = 3),
               valueBoxOutput("topClusterBox", width = 3)
             ),
             fluidRow(
               box(title = "Distribusi Tipe Keuangan Responden", status = "primary", width = 6, plotOutput("marketSegmentPlot")),
               box(title = "Ringkasan Statistik per Segmen", status = "primary", width = 6, tableOutput("marketSummary"))
             ),
             fluidRow(
               box(title = "Pendapatan Bulanan per Tipe Keuangan", status = "info", width = 6, plotOutput("incomeBySegmentPlot")),
               box(title = "Frekuensi Belanja per Tipe Keuangan", status = "info", width = 6, plotOutput("shoppingFreqBySegmentPlot")),
               box(title = "Prioritas Finansial per Tipe Keuangan", status = "info", width = 6, plotOutput("priorityBySegmentPlot")),
               
              fluidRow(
                 box(title = "Income vs Saving Cluster Segmentasi", width = 12, status = "primary",
                     plotOutput("clusterPlot", height = "500px"))
                  
                 )
                 
               )
               
             )
           )
  )

classification_result <- reactiveVal()
classification_image <- reactiveVal()

server <- function(input, output, session) {
  clean_radio_scale <- function(input_value) {
    as.numeric(str_extract(input_value, "^[0-9]"))
  }
  survey_responses_file <- "survey_responses_data.csv"
  
  survey_data <- reactiveVal({
    if (file.exists(survey_responses_file)) {
      df <- read_csv(survey_responses_file, show_col_types = FALSE)
    } else {
      df <- read_csv("financial_behaviour.csv", show_col_types = FALSE)
    }
    
    # Tambahkan kolom kosong investmentAmount kalau belum ada
    if (!"investmentAmount" %in% colnames(df)) {
      df$investmentAmount <- 0
    }
    
    # Konversi kolom numeric
    numeric_cols <- c("income", "expense", "debt", "saving", "shoppingFreq", "investmentAmount")
    numeric_cols_exist <- intersect(numeric_cols, colnames(df))
    
    if (length(numeric_cols_exist) > 0) {
      df <- df %>% mutate(across(all_of(numeric_cols_exist), as.numeric))
    }
    
    df
  })
  
  observeEvent(input$startSurveyHome, {
    print(paste0("startSurveyHome clicked! Navigating to: start_survey_tab"))
    print(paste0("Current main_navbar selected: ", input$main_navbar))
    updateTabsetPanel(session, "main_navbar", selected = "start_survey_tab")
    runjs("$('a[data-value=\"start_survey_tab\"]').tab('show');")
    updateRadioButtons(session, "investment", selected = "Tidak")
    return(NULL)
  })
  
  observeEvent(input$resetSurveyResult, {
    updateNumericInput(session, "income", value = 0)
    updateNumericInput(session, "expense", value = 0)
    updateNumericInput(session, "debt", value = 0)
    updateNumericInput(session, "saving", value = 0)
    updateRadioButtons(session, "shoppingFreq", selected = "2 - Sesekali")
    updateCheckboxInput(session, "investment", value = FALSE)
    updateCheckboxGroupInput(session, "investmentType", selected = character(0))
    updateSliderInput(session, "investmentRisk", value = 3)
    updateNumericInput(session, "investmentAmount", value = 0)
    updateRadioButtons(session, "payment", selected = "QRIS")
    updateSelectInput(session, "shoppingCat", selected = "Makanan/Minuman (Kuliner/Coffee Shop)")
    updateCheckboxGroupInput(session, "insurance", selected = "Tidak Ada")
    updateRadioButtons(session, "recordExpense", selected = "2 - Sesekali")
    updateSelectInput(session, "priority", selected = "Meningkatkan Tabungan Darurat")
    updateTabsetPanel(session, "main_navbar", selected = "start_survey_tab")
  })
  
  observeEvent(input$resetSurveyResult, {
    updateNumericInput(session, "income", value = 0)
    updateNumericInput(session, "expense", value = 0)
    updateNumericInput(session, "debt", value = 0)
    updateNumericInput(session, "saving", value = 0)
    updateRadioButtons(session, "shoppingFreq", selected = "2 - Sesekali")
    updateCheckboxInput(session, "investment", value = FALSE)
    updateCheckboxGroupInput(session, "investmentType", selected = character(0))
    updateSliderInput(session, "investmentRisk", value = 3)
    updateNumericInput(session, "investmentAmount", value = 0)
    updateRadioButtons(session, "payment", selected = "QRIS")
    updateSelectInput(session, "shoppingCat", selected = "Makanan/Minuman (Kuliner/Coffee Shop)")
    updateCheckboxGroupInput(session, "insurance", selected = "Tidak Ada")
    updateRadioButtons(session, "recordExpense", selected = "2 - Sesekali")
    updateSelectInput(session, "priority", selected = "Meningkatkan Tabungan Darurat")
    
    # navigasi ke tab Survey
    updateTabsetPanel(session, "main_navbar", selected = "start_survey_tab")
  })
  
  
  observeEvent(input$submitSurvey, {
    print(paste0("--- SUBMIT SURVEY START ---"))
    print(paste0("investment input saat submit: ", input$investment))
    
    if (input$income <= 0) {
      showModal(modalDialog(title = "Input Tidak Valid", "Pendapatan bulanan harus lebih dari nol untuk analisis.", easyClose = TRUE))
      return()
    }
    
    if (input$expense > input$income * 1.5) {
      showModal(modalDialog(title = "Perhatian", "Pengeluaran Anda jauh melebihi pendapatan.", easyClose = TRUE))
    }
    
    # Prepare data
    input_data_for_prediction_df <- tibble::tibble(
      income = input$income,
      expense = input$expense,
      debt = input$debt,
      saving = input$saving,
      savingRate = input$saving / input$income * 100,
      shoppingFreq = clean_radio_scale(input$shoppingFreq),
      investment = ifelse(is.null(input$investment), "Tidak", input$investment),
      investmentType = if (is.null(input$investmentType) || input$investment == "Tidak") "" else paste(input$investmentType, collapse = "; "),
      investmentRisk = if (input$investment == "Ya") input$investmentRisk else 0,
      investmentAmount = if (input$investment == "Ya") input$investmentAmount else 0,
      payment = input$payment,
      shoppingCat = input$shoppingCat,
      insurance = if (is.null(input$insurance)) "" else paste(input$insurance, collapse = "; "),
      recordExpense = clean_radio_scale(input$recordExpense),
      priority = input$priority
    )
    
    
    # Cek NA
    if (anyNA(input_data_for_prediction_df)) {
      incomplete_cols <- names(input_data_for_prediction_df)[which(sapply(input_data_for_prediction_df, function(x) any(is.na(x))))]
      showModal(modalDialog(
        title = "Data Tidak Lengkap",
        HTML(paste0("Mohon isi seluruh data survei sebelum mengirim.<br><br><strong>Kolom kosong:</strong><br>", paste(incomplete_cols, collapse = "<br>"))),
        easyClose = TRUE
      ))
      return()
    }
    
    # predict
    result <- tryCatch({
      predict(financial_ml_model, newdata = input_data_for_prediction_df)
    }, error = function(e) {
      showModal(modalDialog(
        title = "Prediksi Gagal",
        paste0("Terjadi error saat prediksi: ", e$message),
        easyClose = TRUE
      ))
      return(NULL)
    })
    
    if (is.null(result)) return()
    
    current_classification_str <- as.character(result[1])
    

    # Simpan hasil ke reactiveVal
    current_classification_str <- as.character(result[1])
    classification_result(current_classification_str)
    
    # Set gambar ke reactiveVal image
    img_src <- switch(current_classification_str,
                      "SULTAN" = "sultan.png",
                      "Man In Finance" = "finance.png",
                      "Crypto Bros" = "cryptobros.png",
                      "Jaksel Maksa" = "jaksel.png",
                      "default.png")
    print(paste("IMG PATH:", img_src))
    classification_image(img_src)
    
    # Tampilkan hasil sementara di modal pop up
    showModal(modalDialog(
      title = paste0("Hasil: ", current_classification_str),
      tags$img(src = img_src, height = "180px", style="margin-bottom: 15px;"),
      tags$h4(paste0("Profil keuangan Anda: ", current_classification_str)),
      easyClose = TRUE
    ))

    # Simpan data
    new_entry_full <- input_data_for_prediction_df
    new_entry_full$classification <- current_classification_str
    updated_dashboard_data <- rbind(survey_data(), new_entry_full)
    survey_data(updated_dashboard_data)
    
    # Setelah update survey_data
    updated_dashboard_data <- rbind(survey_data(), new_entry_full)
    survey_data(updated_dashboard_data)
    
    # Cluster ulang data
    clustered_df <- clustered_data()
    
    # Cari cluster untuk user baru
    user_cluster <- clustered_df %>%
      mutate(id = row_number()) %>%
      filter(
        income == input$income,
        expense == input$expense,
        saving == input$saving,
        debt == input$debt
      ) %>%
      slice_tail(n = 1) %>% 
      pull(Cluster)
    
    # Tampilkan ke UI
    output$userClusterResult <- renderText({
      paste0("Kamu masuk dalam Cluster: ", user_cluster)
    })
    
    tryCatch({
      write_csv(updated_dashboard_data, survey_responses_file, append = FALSE)
    }, error = function(e) {
      showModal(modalDialog(
        title = "Gagal Menyimpan Data",
        paste0("Tidak dapat menyimpan data ke file: ", e$message),
        easyClose = TRUE
      ))
    })
    
    
    updateTabsetPanel(session, "main_navbar", selected = "results_tab")
    
    print(paste0("--- SUBMIT SURVEY END ---"))
  })
  
  output$classificationResult <- renderUI({
    req(classification_result())
    req(classification_image())
    
    tagList(
      tags$img(src = classification_image(), height = "200px", style = "margin-bottom: 20px;"),
      tags$h3(paste0("Profil keuangan Anda adalah: ", classification_result()),
              style = "font-weight: bold; color = #007bff;")
    )
  })
  
  aggregated_data <- reactive({
    req(nrow(survey_data()) > 0)
    survey_data() %>%
      group_by(classification) %>%
      summarise(
        Count = n(),
        AvgIncome = mean(as.numeric(income), na.rm = TRUE),
        AvgExpense = mean(as.numeric(expense), na.rm = TRUE),
        AvgDebt = mean(as.numeric(debt), na.rm = TRUE),
        AvgSaving = mean(as.numeric(saving), na.rm = TRUE),
        AvgShoppingFreq = mean(as.numeric(shoppingFreq), na.rm = TRUE),
        AvgInvestmentAmount = mean(as.numeric(investmentAmount), na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(desc(Count))
  })
  

  clustered_data <- reactive({
    req(nrow(survey_data()) > 0)
    df <- survey_data() %>% 
      select(income, expense, debt, saving, shoppingFreq, investmentAmount) %>%
      na.omit()
    
    set.seed(123)
    kmeans_result <- kmeans(scale(df), centers = 4, nstart = 25)
    
    df$Cluster <- as.factor(kmeans_result$cluster)
    df
  })
  
  # Plot cluster
  output$clusterPlot <- renderPlot({
    df <- clustered_data()
    ggplot(df, aes(x = income, y = expense, color = Cluster)) +
      geom_point(size = 3) +
      labs(title = "Cluster Segmentasi Responden",
           x = "Pendapatan Bulanan (Rp)",
           y = "Pengeluaran Bulanan (Rp)") +
      scale_x_continuous(labels = scales::label_dollar(prefix = "Rp", big.mark = ".", decimal.mark = ",")) +
      scale_y_continuous(labels = scales::label_dollar(prefix = "Rp", big.mark = ".", decimal.mark = ",")) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))
  })
  
  output$totalRespondentsBox <- renderValueBox({
    total_respondents <- nrow(survey_data() %>% filter(!is.na(classification)))
    valueBox(value = formatC(total_respondents, format = "f", big.mark = ".", digits = 0), subtitle = "Total Responden", icon = icon("users"), color = "navy")
  })
  output$topSegmentBox <- renderValueBox({
    agg_data <- aggregated_data()
    if (nrow(agg_data) == 0) {
      valueBox(value = "Belum Ada Data", subtitle = "Segmen Terbanyak", icon = icon("star"), color = "olive")
    } else {
      top_segment <- agg_data$classification[1]
      top_segment_count <- agg_data$Count[1]
      valueBox(value = top_segment, subtitle = paste0("Segmen Terbanyak (", top_segment_count, " Responden)"), icon = icon("star"), color = "olive")
    }
  })
  
  output$avgIncomeBox <- renderValueBox({
    req(nrow(survey_data()) > 0)
    avg_income_overall <- mean(survey_data()$income, na.rm = TRUE)
    valueBox(value = scales::dollar(avg_income_overall, prefix = "Rp", big.mark = ".", decimal.mark = ","), subtitle = "Rata-rata Pendapatan (Overall)", icon = icon("wallet"), color = "purple")
  })
  
  output$topClusterBox <- renderValueBox({
    df <- clustered_data()
    cluster_income_avg <- df %>% group_by(Cluster) %>% summarise(AvgIncome = mean(income)) %>% arrange(desc(AvgIncome))
    top_cluster <- cluster_income_avg$Cluster[1]
    top_income <- cluster_income_avg$AvgIncome[1]
    
    valueBox(
      value = paste0("Cluster ", top_cluster),
      subtitle = paste0("Income Tertinggi: Rp ", formatC(top_income, format = "f", big.mark = ".", digits = 0)),
      icon = icon("chart-pie"),
      color = "orange"
    )
  })
  
  
  output$marketSegmentPlot <- renderPlot({
    df <- survey_data()
    req(nrow(df) > 0)
    plot_data <- df %>% count(classification) %>% mutate(percentage = n / sum(n) * 100)
    ggplot(plot_data, aes(x = "", y = percentage, fill = classification)) +
      geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0) +
      geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5), size = 5) +
      labs(title = "Distribusi Tipe Keuangan Responden", fill = "Tipe Keuangan") +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"), legend.position = "right", legend.title = element_text(size = 14), legend.text = element_text(size = 12))
  })
  
  output$clusterPlot <- renderPlot({
    df <- clustered_data()
    ggplot(df, aes(x = income, y = saving, color = Cluster)) +
      geom_point(size = 3, alpha = 0.7) +
      labs(title = "Income vs Saving per Cluster", x = "Pendapatan (Rp)", y = "Tabungan (Rp)") +
      scale_x_continuous(labels = scales::label_dollar(prefix = "Rp", big.mark = ".", decimal.mark = ",")) +
      scale_y_continuous(labels = scales::label_dollar(prefix = "Rp", big.mark = ".", decimal.mark = ",")) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
  })
  
  
  output$marketSummary <- renderTable({
    req(nrow(survey_data()) > 0)
    aggregated_data() %>%
      select(Tipe = classification, Jumlah = Count, `Avg Income (Rp)` = AvgIncome, `Avg Expense (Rp)` = AvgExpense, `Avg Debt (Rp)` = AvgDebt, `Avg Saving (Rp)` = AvgSaving, `Avg Shopping Freq` = AvgShoppingFreq, `Avg Invest Amount (Rp)` = AvgInvestmentAmount)
  }, striped = TRUE, bordered = TRUE, hover = TRUE, align = 'c')
  
  output$incomeBySegmentPlot <- renderPlot({
    df <- survey_data()
    req(nrow(df) > 0)
    ggplot(df, aes(x = classification, y = income, fill = classification)) + geom_boxplot() +
      labs(title = "Pendapatan Bulanan per Tipe Keuangan", x = "Tipe Keuangan", y = "Pendapatan Bulanan (Rp)") +
      scale_y_continuous(labels = scales::label_dollar(prefix = "Rp", big.mark = ".", decimal.mark = ",")) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1, size = 12), axis.text.y = element_text(size = 12), axis.title = element_text(size = 14), legend.position = "none")
  })
  
  output$shoppingFreqBySegmentPlot <- renderPlot({
    df <- survey_data()
    req(nrow(df) > 0)
    ggplot(df, aes(x = classification, y = shoppingFreq, fill = classification)) + geom_boxplot() +
      labs(title = "Frekuensi Belanja per Tipe Keuangan", x = "Tipe Keuangan", y = "Frekuensi Belanja per Bulan (Skala 1-4)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1, size = 12), axis.text.y = element_text(size = 12), axis.title = element_text(size = 14), legend.position = "none")
  })
  
  output$priorityBySegmentPlot <- renderPlot({
    df <- survey_data()
    req(nrow(df) > 0)
    plot_data <- df %>% group_by(classification, priority) %>% summarise(Count = n(), .groups = 'drop') %>% group_by(classification) %>% mutate(Percentage = Count / sum(Count) * 100)
    ggplot(plot_data, aes(x = priority, y = Percentage, fill = classification)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
      geom_text(aes(label = paste0(round(Percentage), "%")), position = position_dodge(width = 0.9), vjust = -0.5, size = 4) +
      labs(title = "Distribusi Prioritas Finansial per Tipe Keuangan", x = "Prioritas Finansial", y = "Persentase (%)", fill = "Tipe Keuangan") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1, size = 12), axis.text.y = element_text(size = 12), axis.title = element_text(size = 14), legend.position = "right")
  })
}

shinyApp(ui, server)

# (Arquivo completo do dashboard Shiny fornecido pelo usuário)
# Manter exatamente o conteúdo original para consistência:
# Você pode substituir este bloco pelo código que enviou (já validado).
# --- INÍCIO ---
# Full rewrite of the Shiny dashboard app (preserves UI layout and behavior)
# Key fixes: unique input ids, safe binding of sensor data, leaflet palette domain,
# load data once globally, replace deprecated ggplot size -> linewidth, .groups = "drop"
#
# Last updated: 2025-11-09 by assistant
#
# Note: test locally. If you deploy to shinyapps.io, confirm fonts and any system packages.
# -----------------------------------------------------------------------------

# LIBRARIES
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinycssloaders)
library(jsonlite)
library(DT)
library(data.table)
library(leaflet)
library(leaflegend)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
library(ggplot2)
library(RColorBrewer)
library(openair)
library(openairmaps)
library(zoo)
library(plotly)
library(lubridate)
library(readr)

# --------------------------------------------------------------------
# LOAD DATA ONCE (global)
# --------------------------------------------------------------------
localizacao <- readRDS(url("https://raw.githubusercontent.com/jessicajcss/ScheduledUpdate/refs/heads/main/data/localizacao.rds"))
Poltab <- readRDS(url("https://raw.githubusercontent.com/jessicajcss/ScheduledUpdate/refs/heads/main/data/pollutants_table.rds"))

load(url("https://github.com/jessicajcss/ScheduledUpdate/raw/refs/heads/main/data/meteo_hour.Rda"))
load(url("https://github.com/jessicajcss/ScheduledUpdate/raw/refs/heads/main/data/data_purpleair.Rda"))
load(url("https://github.com/jessicajcss/ScheduledUpdate/raw/refs/heads/main/data/air_quality_data.Rda"))
load(url("https://github.com/jessicajcss/ScheduledUpdate/raw/refs/heads/main/data/air_quality_data_ugm3.Rda"))
load(url("https://github.com/jessicajcss/ScheduledUpdate/raw/refs/heads/main/data/data_thermo_instantaneo_ugm3.Rda"))
AQItab <- readRDS(url("https://github.com/jessicajcss/ScheduledUpdate/raw/refs/heads/main/data/AQItab.rds"))

# --------------------------------------------------------------------
# PREPROCESSING (adapted from your original)
# --------------------------------------------------------------------

# THERMO localizacao selection (keeps City order)
thermo_localizacao <- localizacao %>%
  filter((Local == "Defesa Civil" | Local == "Prefeitura") & Tipo == "outdoor") %>%
  filter(Cidade %in% c("Almirante Tamandaré", "Rio Branco do Sul")) %>%
  select(Cidade, Lat, Long) %>%
  mutate(Cidade = factor(Cidade, levels = c("Rio Branco do Sul", "Almirante Tamandaré"))) %>%
  rename(Latitude = Lat, Longitude = Long)

# purpleair localizacao join helper
localizacao_purpleair <- localizacao %>%
  mutate(sensor_id = Local) %>%
  select(-Local)

# METEO: adjust timezone and rename Date
meteo <- meteo_hour %>%
  mutate(date = as_datetime(date, tz = "America/Sao_Paulo")) %>%
  filter(Cidade %in% c("Rio Branco do Sul", "Colombo")) %>%
  rename(Date = date) %>%
  mutate(Date = as_datetime(Date, tz = "America/Sao_Paulo"),
         Cidade = case_when(Cidade == "Colombo" ~ "Almirante Tamandaré", TRUE ~ Cidade)) %>%
  mutate(Cidade = factor(Cidade, levels = c("Rio Branco do Sul", "Almirante Tamandaré")))

# PURPLEAIR: align column names and location
purpleair <- data_purpleair %>%
  mutate(Date = sample_day,
         AQI = AQI_PM2.5) %>%
  select(-sample_day) %>%
  left_join(., localizacao_purpleair, by = c("Cidade", "sensor_id", "Tipo")) %>%
  filter(Tipo == "outdoor") %>%
  rename(AQI_PM25 = AQI_PM2.5,
         Latitude = Lat,
         Longitude = Long) %>%
  # ensure consistent pollutant naming (purpleair primarily has PM2.5)
  mutate(PM2.5 = ifelse(is.na(`PM2.5`) & !is.na(PM2.5), PM2.5, `PM2.5`)) %>%
  select(everything())

# THERMO Datafinal: convert units (as in original) and add location
Datafinal <- air_quality_data %>%
  mutate(CO = CO * 1.15,
         O3 = O3 * 1.96,
         NO2 = NO2 * 1.88,
         SO2 = SO2 * 2.62,
         PM10 = PM10) %>%
  rename(Date = sample_day) %>%
  left_join(., thermo_localizacao, by = "Cidade") %>%
  mutate(Cidade = factor(Cidade, levels = c("Rio Branco do Sul", "Almirante Tamandaré")),
         Year = format(Date, "%Y"),
         Tipo = coalesce(Tipo, "outdoor"))

# HOURLY concentration data (ug/m3)
data <- air_quality_data_ugm3 %>%
  mutate(Year = as.factor(year(date))) %>%
  pivot_longer(-c('Cidade', 'Year', 'date'), values_to = "Concentração (ug/m³)", names_to = "Poluente")

# INSTANTANEO data
df_instantaneo <- data_thermo_instantaneo %>%
  mutate(Year = as.factor(year(date))) %>%
  pivot_longer(-c('Cidade', 'Year', 'date'), values_to = "Concentração (ug/m³)", names_to = "Poluente")

# Ensure tz on relevant date columns
if ("date" %in% names(df_instantaneo)) tz(df_instantaneo$date) <- "America/Sao_Paulo"
if ("date" %in% names(data)) tz(data$date) <- "America/Sao_Paulo"

# Guarantee a consistent set of column names for binding Datafinal and purpleair
# We'll create an ordered union of columns and ensure both datasets have them
all_cols <- union(names(Datafinal), names(purpleair))

Datafinal_safe <- Datafinal %>%
  mutate(AQI = as.numeric(AQI),
         Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude)) %>%
  select(all_of(all_cols))

purpleair_safe <- purpleair %>%
  mutate(AQI = as.numeric(AQI),
         Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude)) %>%
  select(all_of(all_cols))

# Bind sensors safely and drop rows with missing coords
alldata <- bind_rows(Datafinal_safe, purpleair_safe) %>%
  mutate(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude)) %>%
  filter(!is.na(Latitude) & !is.na(Longitude))

# Additional helper variables (for UI choices)
Year <- unique(Datafinal$Year)
Cidade <- unique(Datafinal$Cidade)
Locais <- unique(as.factor(localizacao$Local))
Cities <- unique(as.factor(localizacao$Cidade))

# POLLUTANT precaution tables (Poltab already loaded)
pm2_5data <- Poltab[Poltab$Poluente == " PM2.5", ]
pm10data   <- Poltab[Poltab$Poluente == " PM10", ]
no2data    <- Poltab[Poltab$Poluente == " NO2", ]
codata     <- Poltab[Poltab$Poluente == " CO", ]
so2data    <- Poltab[Poltab$Poluente == " SO2", ]
o3data     <- Poltab[Poltab$Poluente == " O3", ]

# -----------------------------------------------------------------------------
# UI (kept overall layout and content, changed duplicate input ids)
# -----------------------------------------------------------------------------
title <- tags$img(src='https://raw.githubusercontent.com/jessicajcss/Shiny_RMC/main/www/ufpr.png',
                  height='30', "Qualidade do Ar & Meteorologia", align = "left")

ui <- dashboardPage(
  skin = 'blue',
  dashboardHeader(title = "Qualidade do Ar & Meteorologia", titleWidth = 330),
  dashboardSidebar(
    width = 330,
    sidebarMenu(
      menuItem("Página Inicial", tabName = "home", icon = icon('home')),
      menuItem(("Qualidade do ar"),tabName = "dashboard",icon=icon('map')),
      menuItem(HTML((paste0("MP",tags$sub("2.5"), " em tempo real"))), tabName = "purpleair",icon = icon('map')),
      menuItem(HTML((paste0("Partículas e gases em tempo real"))), tabName = "thermo", icon = icon('clock')),
      menuItem(("Variação Temporal"), tabName = "Line_graph",icon = icon('chart-line')),
      menuItem(("Tendências de Poluição"), tabName = "year_data", icon = icon('th')),
      menuItem("Dados Brutos",tabName = "table",icon = icon('table'))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(
        HTML(" #compare_state_option,#compare_year_option ,.compare-county-wrapper { display:flex; margin-bottom:-10px;}
    				#compare_state_option > div, #compare_year_option > div, .compare-county-wrapper > div > div {padding-right: 15px;}
    				.shiny-output-error:before,.shiny-output-error{visibility: hidden !important;}
    				.compare-class > .tabbable > .nav-tabs {margin-bottom: 20px !important;}
    				.box.box-solid.box-primary {border: 1px solid #dd4b39 !important;}
    				.box.box-solid.box-primary>.box-header { background: #dd4b39 !important; background-color: #dd4b39 !important; }
    				.sidebar-menu>li {font-size:17px;}")
      )
    ),
    tabItems(
      # RAW DATA TAB
      tabItem(tabName = "table",
              tags$h3('Download de dados', width = 3),
              fluidRow(
                column(3,
                       dateInput("start_date",h3("Data inicial"),
                                 format = "yyyy-mm-dd",
                                 value = "2023-08-10",
                                 min = "2023-08-10",
                                 max = "2025-10-24"),
                       dateInput("end_date",h3("Data final"),
                                 format = "yyyy-mm-dd",
                                 value = "2025-10-24",
                                 min = "2023-08-10",
                                 max = "2025-10-24")),
                column(9,
                       tableOutput("tableData"))
              ),
              br(), br(),
              downloadButton("downloadData")
      ),

      # HOME TAB
      tabItem(tabName = "home",
              fluidRow(
                column(width = 7, align = "center", tags$h2("Inventário de material particulado em municípios com atividades minerais estabelecidas na Região Metropolitana de Curitiba - Paraná, Brasil")),
                HTML('<html>
            <head>
              <style>
                table { font-family: arial, sans-serif; border-collapse: collapse; width: 100%; }
                td, th { border: 1px solid #dddddd; text-align: left; padding: 8px; }
                tr:nth-child(even) { background-color: #dddddd; }
                h3, h2 { font-weight: bold; }
                p { font-size: 19px; }
              </style>
            </head>
          </html>'),
                column(width = 5, align = "center", tags$img(src = "https://raw.githubusercontent.com/jessicajcss/Shiny_RMC/main/www/labair.png", height = 200))
              ),
              br(),
              fluidRow(column(width = 12, tags$h2("Página do Projeto"))),
              fluidRow(box(width = 12, tags$iframe(seamless = "seamless", src = "https://rmcqualidadedoar.netlify.app/", height = 430, width = '100%'))),
              fluidRow(column(width = 12, tags$h2("Panorama da Pesquisa")),
                       box(width = 12, HTML('<iframe width="100%" height="430" src="https://ai.invideo.io/watch/JAUHnJYsGc-" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))),
              br(),
              fluidRow(column(width = 12, tags$h2("Locais de Monitoramento")),
                       box(title = "Qualidade do Ar e Meteorológico", solidHeader = TRUE, status = "primary", height = 650, width = 12, leafletOutput(height = 590, "sites")))
      ),

      # PURPLEAIR PAGE
      tabItem(tabName = "purpleair",
              fluidRow(box(width = 12, tags$iframe(seamless = "seamless", src = "https://map.purpleair.com/1/lt/mPM25/a525600/p86400/cC5#9.7/-25.3517/-49.2683", height = 800, width = "100%")))),

      # MAP AND BAR GRAPH TAB
      tabItem(tabName = "dashboard",
              fluidRow(
                column(3,
                       dateInput("select_date", h3("Selecione a Data"), format = "yyyy-mm-dd", value = "2025-10-24", min = "2023-08-10", max = "2025-10-24")),
                column(9, align = "right",
                       div(tags$em("Acesse nosso canal com boletim diário sobre a qualidade do ar"), style = "margin-top: 10px; font-size: 14px;"),
                       tags$a(href = "https://whatsapp.com/channel/0029Vb8tI1b6rsQjZOJtci0r", target = "_blank", tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/6/6b/WhatsApp.svg", height = "40", style = "margin-top: 5px;")))),
              fluidRow(
                box(title = "Mapa (IQA Geral)", solidHeader = TRUE, status = "primary", height = 650, width = 7, leafletOutput(height = 590, "map")),
                box(title = "Distribuição dos poluentes", solidHeader = TRUE, status = "primary", width = 5,
                    tabBox(width = 12,
                           tabPanel(title = HTML((paste0("SO",tags$sub("2")))), plotOutput(height = 500, "plotSO2")),
                           tabPanel(title = HTML((paste0("NO",tags$sub("2")))), plotOutput(height = 500, "plotNO2")),
                           tabPanel(title = HTML((paste0("O",tags$sub("3")))), plotOutput(height = 500, "plotO3")),
                           tabPanel(title = "CO", plotOutput(height = 500, "plotCO")),
                           tabPanel(title = HTML((paste0("MP",tags$sub("2.5")))), plotOutput(height = 500, "plotPM25")),
                           tabPanel(title = HTML((paste0("MP",tags$sub("10")))), plotOutput(height = 500, "plotPM10")))))
              ,
              br(),
              fluidRow(box(title = "Índice de Qualidade do Ar (IQA)",
                           footer = "Referências: Tabela <https://aqicn.org/scale/pt/>;   Cálculos baseados no guia da US EPA (2024) <https://document.airnow.gov/technical-assistance-document-for-the-reporting-of-daily-air-quailty.pdf>",
                           width = 12, tags$img(width = '90%', src = "https://raw.githubusercontent.com/jessicajcss/Shiny_RMC/main/www/aqi_table.png", align = "center")))
      ),

      # LINE GRAPH TAB
      tabItem(tabName = "Line_graph",
              fluidRow(
                column(3,
                       box(title = "Inputs", solidHeader = TRUE, status = "primary", width = 12, height = 600,
                           selectInput("Cities1", h3("Escolha uma Cidade"), Cidade, selected = 'Rio Branco do Sul'),
                           radioButtons("Poluente", "Poluente:",
                                        choiceNames = list(HTML((paste0("SO",tags$sub("2")))), HTML((paste0("NO",tags$sub("2")))), HTML((paste0("O",tags$sub("3")))), "CO", HTML((paste0("MP",tags$sub("2.5")))), HTML((paste0("MP",tags$sub("10"))))),
                                        choiceValues = list("SO2", "NO2", "O3", "CO", "PM2.5", "PM10")),
                           dateInput("start_date3", h3("Data inicial"), format = "yyyy-mm-dd", value = ymd("2025-10-24") - 30, min = "2023-08-10", max = "2025-10-24"),
                           dateInput("end_date3", h3("Data final"), format = "yyyy-mm-dd", value = "2025-10-24", min = "2023-08-10", max = "2025-10-24")
                       )
                ),
                column(9, box(title = "Série Temporal", solidHeader = TRUE, status = "primary", width = 12, height = 600, box(width = 12, plotOutput(height = 500, "plots")))),
                box(title = "Concentrações e ventos", solidHeader = TRUE, status = "primary", height = 650, width = 12, leafletOutput(height = 590, "map_polarplot"))
              ),
              fluidRow(box(title = "Condições Meteorológicas", status = "primary", solidHeader = TRUE, plotOutput("dist"), width = 8),
                       box(title = "Rosa dos ventos", status = "primary", solidHeader = TRUE, plotOutput("wrose"), width = 4))
      ),

      # THERMO REAL-TIME
      tabItem(tabName = "thermo",
              fluidRow(
                box(title = "Sensores de gases e partículas", solidHeader = TRUE, status = "primary", width = 12,
                    mainPanel(plotlyOutput('plot'), width = 12),
                    fluidRow(
                      column(2),
                      column(4,
                             selectInput("city_thermo", "Escolha a Cidade:", choices = unique(df_instantaneo$Cidade), selected = 'Rio Branco do Sul')),
                      column(2,
                             dateInput("start_date2","Data inicial", format = "yyyy-mm-dd", value = ymd("2025-10-24") - 7, min = "2023-08-10", max = "2025-10-24")),
                      column(2,
                             dateInput("end_date2","Data final", format = "yyyy-mm-dd", value = "2025-10-24", min = "2023-08-10", max = "2025-10-24"))
                    ))
              )),

      # CORRELATION MATRIX TAB
      tabItem(tabName = "year_data",
              fluidRow(column(4, selectInput("city_year", ("Escolha uma Cidade:"), Cidade, selected = 'Rio Branco do Sul')),
                       column(8, selectInput("years", ("Escolha um Ano:"), Year, selected = "2024"))),
              fluidRow(
                column(6,
                       box(title = "Matriz de Correlação", solidHeader = TRUE, status = "primary", width = 12,
                           tabsetPanel(tabPanel("Coeficientes de correlação", withSpinner(plotOutput("corrcoeff", height = 475))),
                                       tabPanel("Correlação", withSpinner(plotOutput("corrscatt", height = 475))),
                                       tabPanel("Heat map", withSpinner(plotOutput("heatmap", height = 475)))
                           ))),
                column(6,
                       box(title = "Risco e Prevenção", solidHeader = TRUE, status = "primary", width = "90%",
                           tabsetPanel(footer = "Referência: <https://portal.ct.gov/deep/air/monitoring/aqi-health-effects-statements>",
                                       tabPanel(HTML(c(paste0("SO",tags$sub("2")))), withSpinner(dataTableOutput("tabSO2", height = 475))),
                                       tabPanel(HTML(c(paste0("NO",tags$sub("2")))), withSpinner(dataTableOutput("tabNO2", height = 475))),
                                       tabPanel(HTML(c(paste0("O",tags$sub("3")))), withSpinner(dataTableOutput("tabO3", height = 475))),
                                       tabPanel("CO", withSpinner(dataTableOutput("tabCO", height = 475))),
                                       tabPanel(HTML(c(paste0("MP",tags$sub("2.5")))), withSpinner(dataTableOutput("tabPM25", height = 475))),
                                       tabPanel(HTML(c(paste0("MP",tags$sub("10")))), withSpinner(dataTableOutput("tabPM10", height = 475)))
                           ))))
      )
    )
  )
)

# -----------------------------------------------------------------------------
# SERVER
# -----------------------------------------------------------------------------
server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # THERMO REAL-TIME: plotly scatter of instantaneo measurements
  # ---------------------------------------------------------------------------
  df_instant_lim <- df_instantaneo %>%
    mutate(value = `Concentração (ug/m³)`,
           limite = case_when(
             Poluente == 'SO2' & value >= 40 ~ 1,
             Poluente == 'NO2' & value >= 25 ~ 1,
             Poluente == 'PM10' & value >= 45 ~ 1,
             Poluente == 'PM2.5' & value >= 15 ~ 1,
             Poluente == 'CO' & value >= 4 ~ 1,
             TRUE ~ 0
           ))

  output$plot <- renderPlotly({
    req(input$city_thermo, input$start_date2, input$end_date2)
    start_dt <- as.POSIXct(input$start_date2, tz = "America/Sao_Paulo")
    end_dt   <- as.POSIXct(input$end_date2, tz = "America/Sao_Paulo") + days(1)

    plot_df <- df_instant_lim %>%
      filter(Poluente != "rh_sensor",
             Cidade == input$city_thermo,
             date >= start_dt,
             date <= end_dt) %>%
      arrange(date)

    if (nrow(plot_df) == 0) {
      return(plotly_empty())
    }

    p <- plot_ly(data = plot_df,
                 x = ~date,
                 y = ~`Concentração (ug/m³)`,
                 type = 'scatter',
                 mode = 'markers',
                 color = ~Poluente,
                 colors = 'Paired',
                 marker = list(opacity = 0.6,
                               sizemode = 'diameter',
                               size = ~pmax(1, 0.3 * `Concentração (ug/m³)`),
                               sizeref = 1.2)
    ) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Concentração (µg/m³), exceto CO (mg/m³)"),
             legend = list(title = list(text = "<b>Selecione o poluente</b>"),
                           orientation = "h", x = 0.5, y = 1.15, xanchor = "center", yanchor = "bottom",
                           bgcolor = "rgba(255,255,255,0.7)"))
    p %>% plotly::toWebGL()
  })

  # ---------------------------------------------------------------------------
  # CORRELATION MATRIX (year_data tab)
  # ---------------------------------------------------------------------------
  output$corrcoeff <- renderPlot({
    req(input$city_year, input$years)
    mydata2 <- air_quality_data_ugm3 %>%
      mutate(Year = format(date, "%Y")) %>%
      filter(Year == input$years, Cidade == input$city_year)

    if (nrow(mydata2) == 0) return(plot.new())

    mydata <- mydata2[, c(3:8)]
    mydata.rcorr <- Hmisc::rcorr(as.matrix(mydata))
    corrplot::corrplot(mydata.rcorr$r, method = "number")
  })

  output$corrscatt <- renderPlot({
    req(input$city_year, input$years)
    mydata2 <- air_quality_data_ugm3 %>%
      mutate(Year = format(date, "%Y")) %>%
      filter(Year == input$years, Cidade == input$city_year)

    mydata <- mydata2[, c(3:8)]
    if (nrow(mydata) < 2) return(plot.new())
    PerformanceAnalytics::chart.Correlation(mydata, histogram = TRUE, pch = 19)
  })

  output$heatmap <- renderPlot({
    req(input$city_year, input$years)
    mydata2 <- air_quality_data_ugm3 %>%
      mutate(Year = format(date, "%Y")) %>%
      filter(Year == input$years, Cidade == input$city_year)

    mydata <- mydata2[, c(3:8)]
    mydata.rcorr <- Hmisc::rcorr(as.matrix(mydata))
    palette <- colorRampPalette(c("green", "white", "red"))(20)
    heatmap(x = mydata.rcorr$r, col = palette, symm = TRUE)
  })

  # ---------------------------------------------------------------------------
  # POLLUTANT precaution tables
  # ---------------------------------------------------------------------------
  output$tabPM25 <- DT::renderDataTable({
    datatable(pm2_5data[, c(1:3)], options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE), rownames = FALSE)
  })
  output$tabPM10 <- DT::renderDataTable({
    datatable(pm10data[, c(1:3)], options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE), rownames = FALSE)
  })
  output$tabNO2 <- DT::renderDataTable({
    datatable(no2data[, c(1:3)], options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE), rownames = FALSE)
  })
  output$tabCO <- DT::renderDataTable({
    datatable(codata[, c(1:3)], options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE), rownames = FALSE)
  })
  output$tabSO2 <- DT::renderDataTable({
    datatable(so2data[, c(1:3)], options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE), rownames = FALSE)
  })
  output$tabO3 <- DT::renderDataTable({
    datatable(o3data[, c(1:3)], options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE), rownames = FALSE)
  })

  # ---------------------------------------------------------------------------
  # AQI table (static image and table)
  # ---------------------------------------------------------------------------
  output$tableAQI <- renderTable({ AQItab }, striped = TRUE, spacing = 'm', width = '85%', align = 'c')

  # ---------------------------------------------------------------------------
  # MAP FOR AQI (dashboard tab)
  # ---------------------------------------------------------------------------
  output$map <- renderLeaflet({
    req(input$select_date)
    Day <- alldata %>%
      filter(as.Date(Date) == as.Date(input$select_date)) %>%
      mutate(sensor = ifelse(!is.na(SO2), "GM-5000", "PurpleAir"),
             popup_Info = paste0("Cidade: ", Cidade, "</br>",
                                 "Ambiente: ", coalesce(Tipo, "outdoor"), "</br>",
                                 "IQA: ", ifelse(!is.na(AQI), round(AQI, 1), ifelse(!is.na(AQI_PM25), round(AQI_PM25, 1), "NA")), "</br>",
                                 "Condition: ", coalesce(AQI_Qualidade, ""), "</br>",
                                 "PM2.5 (ug/m³): ", ifelse(!is.na(`PM2.5`), round(`PM2.5`, 1), ifelse(!is.na(PM2.5), round(PM2.5, 1), NA)), "</br>",
                                 "Sensor: ", sensor))

    if (nrow(Day) == 0) {
      leaflet() %>% addTiles()
    } else {
      # determine AQI domain for palette (fallback)
      aqi_domain <- Day$AQI
      if (all(is.na(aqi_domain))) {
        # fallback to AQI_PM25 values or default range
        aqi_domain <- coalesce(Day$AQI_PM25, c(0, 300))
      }
      risk.bins <- c(0, 50, 100, 150, 200, 300)
      pal <- colorBin(colorRampPalette(c("#5F0FA2", "#814FA7", "#F46D43", "#FDAE61", "yellow", "#ABDDA4"))(length(risk.bins)-1),
                      domain = aqi_domain, bins = risk.bins, reverse = TRUE)

      leaflet(data = Day) %>%
        addTiles() %>%
        addCircleMarkers(lat = ~Latitude, lng = ~Longitude,
                         opacity = 0.5, fillOpacity = 1,
                         color = ~ifelse(!is.na(SO2), "black", "gray"),
                         stroke = TRUE, weight = 3,
                         radius = 15, popup = ~popup_Info,
                         fillColor = ~pal(AQI))
    }
  })

  # ---------------------------------------------------------------------------
  # BAR PLOTS FOR INDIVIDUAL POLLUTANTS (dashboard tab)
  # ---------------------------------------------------------------------------

  output$plotPM25 <- renderPlot({
    req(input$select_date)
    Day <- alldata %>%
      filter(as.Date(Date) == as.Date(input$select_date)) %>%
      group_by(Cidade, Date) %>%
      summarise(`PM2.5` = mean(coalesce(`PM2.5`, PM2.5), na.rm = TRUE),
                AQI_PM25 = mean(coalesce(AQI_PM25, AQI), na.rm = TRUE),
                .groups = "drop")

    if (nrow(Day) == 0) return(NULL)

    risk.bins <- c(0/300, 50/300, 100/300, 150/300, 200/300, 300/300)
    color_palette <- rev(c("#5F0FA2", "#814FA7", "#814FA7", "#F46D43", "#FDAE61", "#FDAE61", "yellow", "yellow", "#ABDDA4", "#ABDDA4"))

    ggplot(Day, aes(x = Cidade, y = `PM2.5`, fill = AQI_PM25)) +
      geom_col(width = 0.5) +
      theme_classic() +
      scale_fill_gradientn(colors = color_palette, values = risk.bins, limits = c(0, 300), na.value = "white", name = expression(paste("IQA ", MP[2.5]))) +
      scale_y_continuous(breaks = seq(0, 30, 5)) +
      geom_hline(yintercept = 15, linewidth = 0.5, color = "red3") +
      annotate("text", x = 0.5, y = 15 * 1.02, color = "red3", label = expression("Referência OMS 24h"), size = 5, hjust = 0) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      ylab(expression(paste("Concentração de ", MP[2.5], " (", mu, "g ", m^-3, ")")))
  })

  output$plotPM10 <- renderPlot({
    req(input$select_date)
    Day <- Datafinal %>% filter(as.Date(Date) == as.Date(input$select_date))

    if (nrow(Day) == 0) return(NULL)

    risk.bins <- c(0/300, 50/300, 100/300, 150/300, 200/300, 300/300)
    color_palette <- rev(c("#5F0FA2", "#814FA7", "#814FA7", "#F46D43", "#FDAE61", "#FDAE61", "yellow", "yellow", "#ABDDA4", "#ABDDA4"))

    ggplot(Day, aes(x = Cidade, y = PM10, fill = AQI_PM10)) +
      geom_col(width = 0.5) +
      theme_classic() +
      scale_fill_gradientn(colors = color_palette, values = risk.bins, limits = c(0, 300), na.value = "white", name = expression(paste("IQA ", MP[10]))) +
      scale_y_continuous(breaks = seq(0, 150, 25)) +
      geom_hline(yintercept = 45, linewidth = 0.5, color = "red3") +
      annotate("text", x = 0.5, y = 45 * 1.02, color = "red3", label = expression("Referência OMS 24h"), size = 5, hjust = 0) +
      ylab(expression(paste("Concentração de ", MP[10], " (", mu, "g ", m^-3, ")")))
  })

  output$plotNO2 <- renderPlot({
    req(input$select_date)
    Day <- Datafinal %>% filter(as.Date(Date) == as.Date(input$select_date))
    if (nrow(Day) == 0) return(NULL)

    risk.bins <- c(0/300, 50/300, 100/300, 150/300, 200/300, 300/300)
    color_palette <- rev(c("#5F0FA2", "#814FA7", "#814FA7", "#F46D43", "#FDAE61", "#FDAE61", "yellow", "yellow", "#ABDDA4", "#ABDDA4"))

    ggplot(Day, aes(x = Cidade, y = NO2, fill = AQI_NO2)) +
      geom_col(width = 0.5) +
      theme_classic() +
      scale_fill_gradientn(colors = color_palette, values = risk.bins, limits = c(0, 300), name = expression(paste("IQA ", NO[2]))) +
      scale_y_continuous(breaks = seq(0, max(Datafinal$NO2, na.rm = TRUE) * 5, 25)) +
      geom_hline(yintercept = 25, linewidth = 0.5, color = "red3") +
      annotate("text", x = 0.5, y = 25 * 1.07, color = "red3", label = expression("Referência OMS 24h"), size = 5, hjust = 0) +
      ylab(expression(paste("Concentração de ", NO[2], " (", mu, "g ", m^-3, ")")))
  })

  output$plotCO <- renderPlot({
    req(input$select_date)
    Day <- Datafinal %>% filter(as.Date(Date) == as.Date(input$select_date))
    if (nrow(Day) == 0) return(NULL)

    risk.bins <- c(0/300, 50/300, 100/300, 150/300, 200/300, 300/300)
    color_palette <- rev(c("#5F0FA2", "#814FA7", "#814FA7", "#F46D43", "#FDAE61", "#FDAE61", "yellow", "yellow", "#ABDDA4", "#ABDDA4"))

    ggplot(Day, aes(x = Cidade, y = CO, fill = AQI_CO)) +
      geom_col(width = 0.5) +
      theme_classic() +
      scale_fill_gradientn(colors = color_palette, values = risk.bins, limits = c(0, 300), name = expression(paste("IQA CO"))) +
      scale_y_continuous(breaks = seq(0, 5, 1)) +
      geom_hline(yintercept = 4, linewidth = 0.5, color = "red3") +
      annotate("text", x = 0.5, y = 4 * 1.02, color = "red3", label = expression("Referência OMS 8h"), size = 5, hjust = 0) +
      ylab(expression(paste("Concentração de CO (mg ", m^-3, ")")))
  })

  output$plotSO2 <- renderPlot({
    req(input$select_date)
    Day <- Datafinal %>% filter(as.Date(Date) == as.Date(input$select_date))
    if (nrow(Day) == 0) return(NULL)

    risk.bins <- c(0/300, 50/300, 100/300, 150/300, 200/300, 300/300)
    color_palette <- rev(c("#5F0FA2", "#814FA7", "#814FA7", "#F46D43", "#FDAE61", "#FDAE61", "yellow", "yellow", "#ABDDA4", "#ABDDA4"))

    ggplot(Day, aes(x = Cidade, y = SO2, fill = AQI_SO2)) +
      geom_col(width = 0.5) +
      theme_classic() +
      scale_fill_gradientn(colors = color_palette, values = risk.bins, limits = c(0, 300), name = expression(paste("IQA ", SO[2]))) +
      scale_y_continuous(breaks = seq(0, max(Datafinal$SO2, na.rm = TRUE) * 5, 20)) +
      geom_hline(yintercept = 40, linewidth = 0.5, color = "red3") +
      annotate("text", x = 0.5, y = 40 * 1.05, color = "red3", label = expression("Referência OMS 24h"), size = 5, hjust = 0) +
      ylab(expression(paste("Concentração de ", SO[2], " (", mu, "g ", m^-3, ")")))
  })

  output$plotO3 <- renderPlot({
    req(input$select_date)
    Day <- Datafinal %>% filter(as.Date(Date) == as.Date(input$select_date))
    if (nrow(Day) == 0) return(NULL)

    risk.bins <- c(0/300, 50/300, 100/300, 150/300, 200/300, 300/300)
    color_palette <- rev(c("#5F0FA2", "#814FA7", "#814FA7", "#F46D43", "#FDAE61", "#FDAE61", "yellow", "yellow", "#ABDDA4", "#ABDDA4"))

    ggplot(Day, aes(x = Cidade, y = O3, fill = AQI_O3)) +
      geom_col(width = 0.5) +
      theme_classic() +
      scale_fill_gradientn(colors = color_palette, values = risk.bins, limits = c(0, 300), name = expression(paste("IQA ", O[3]))) +
      scale_y_continuous(breaks = seq(0, max(Datafinal$O3, na.rm = TRUE) * 3, 25)) +
      geom_hline(yintercept = 100, linewidth = 0.5, color = "red3") +
      annotate("text", x = 0.5, y = 100 * 1.02, color = "red3", label = expression("Referência OMS 8h"), size = 5, hjust = 0) +
      ylab(expression(paste("Concentração de ", O[3], " (", mu, "g ", m^-3, ")")))
  })

  # ---------------------------------------------------------------------------
  # LINE GRAPHS (Line_graph tab)
  # ---------------------------------------------------------------------------
  Datafinal <- Datafinal %>% mutate(Cidade = factor(Cidade, levels = c("Rio Branco do Sul", "Almirante Tamandaré")))

  output$plots <- renderPlot({
    req(input$start_date3, input$end_date3, input$Cities1)
    Datafinal <- Datafinal %>% mutate(Date = as.Date(Date, tz = "America/Sao_Paulo"))

    week_new <- Datafinal %>%
      select(Cidade, Date, SO2, NO2, PM2.5 = `PM2.5`, PM10, O3, CO) %>%
      filter(between(Date, as.Date(input$start_date3), as.Date(input$end_date3)))

    week_Cidade <- week_new %>%
      filter(Cidade == input$Cities1) %>%
      group_by(Date) %>%
      summarise(across(SO2:CO, ~ mean(.x, na.rm = TRUE)), .groups = "drop")

    week_other <- week_new %>%
      filter(Cidade != input$Cities1) %>%
      group_by(Date) %>%
      summarise(across(SO2:CO, ~ mean(.x, na.rm = TRUE)), .groups = "drop")

    # pick pollutant column to plot
    pol <- input$Poluente
    pol_col <- case_when(
      pol == "PM2.5" ~ "PM2.5",
      pol == "PM10" ~ "PM10",
      pol == "NO2" ~ "NO2",
      pol == "CO" ~ "CO",
      pol == "SO2" ~ "SO2",
      pol == "O3" ~ "O3",
      TRUE ~ "PM2.5"
    )

    x <- week_Cidade %>% select(Date, all_of(pol_col))
    y <- week_other %>% select(Date, all_of(pol_col))

    plot(x[[2]], type = "b", lwd = 2, xaxt = "n",
         ylim = c(0, max(Datafinal[[pol_col]], na.rm = TRUE)), col = "green",
         xlab = "Data", ylab = "Concentração (ug/m³)")
    # overlay other city
    if (nrow(y) > 0) {
      lines(y[[2]], col = "black", type = "b", lwd = 2)
    }
    title(input$Cities1, col.main = "green")
    axis.Date(1, at = seq(as.Date(input$start_date3), as.Date(input$end_date3), by = "days"),
              format = "%b %d \n %Y", cex.axis = .7)

    legend("topright", legend = c(expression(SO["2"]), expression(NO["2"]), expression(O["3"]), "CO", expression(MP["2,5"]), expression(MP["10"])),
           lty = 5, lwd = 4, pch = 10,
           col = c("orange", "red", "grey50", "blue", "green", "brown"),
           ncol = 2, bty = "n", cex = 0.8)
  })

  # ---------------------------------------------------------------------------
  # MAP FOR SITES (home tab)
  # ---------------------------------------------------------------------------
  output$sites <- renderLeaflet({
    # small manual DF for special stations used originally
    df <- data.frame(Latitute = c(-25.322265201285667, -25.193976227163617),
                     Longitude = c(-49.1578184144157, -49.311729610632256),
                     ticker = c("Colombo", "Rio Branco do Sul"),
                     Estacao = c("Estação Meteorológica em Colombo", "Estação Meteorológica em Rio Branco do Sul"),
                     stringsAsFactors = FALSE)

    IconSet <- awesomeIconList(
      'Rio Branco do Sul' = makeAwesomeIcon(icon = 'cloud', squareMarker = FALSE, library = 'glyphicon', markerColor = 'red', iconColor = 'white'),
      'Colombo' = makeAwesomeIcon(icon = 'cloud', squareMarker = FALSE, library = 'glyphicon', markerColor = 'green', iconColor = 'white')
    )

    localizacao2 <- localizacao %>%
      mutate(site = as.factor("Local de Amostragem"))

    leaflet(df) %>%
      addTiles() %>%
      addAwesomeMarkers(lng = ~Longitude, lat = ~Latitute, icon = ~IconSet[ticker], label = ~ Estacao) %>%
      addCircleMarkers(lng = localizacao2$Long, lat = localizacao2$Lat, group = localizacao2$Cidade,
                       fillColor = "black", color = "black",
                       label = ~ paste("Estação de Monitoramento:", localizacao2$Local, "-", localizacao2$Cidade)) %>%
      addLegendFactor(values = ~localizacao2$site, pal = colorFactor("black", localizacao2$site), title = "Pontos de Monitoramento", opacity = 0.5, position = "bottomleft") %>%
      addLegendAwesomeIcon(iconSet = IconSet, orientation = 'horizontal',
                           title = htmltools::tags$div(style = 'font-size: 12px;', 'Estações Meteorológicas'),
                           position = 'bottomleft') %>%
      addLayersControl(overlayGroups = unique(localizacao2$Cidade), options = layersControlOptions(collapsed = FALSE)) %>%
      addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron") %>%
      addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
      addLayersControl(baseGroups = c("CartoDB.Positron", "Esri.WorldImagery"), position = "bottomleft")
  })

  # ---------------------------------------------------------------------------
  # POLAR MAP and WIND ROSE (Line_graph tab)
  # ---------------------------------------------------------------------------
  output$map_polarplot <- renderLeaflet({
    req(input$start_date3, input$end_date3)
    pol_data <- meteo %>%
      mutate(date = Date) %>%
      left_join(air_quality_data_ugm3, ., by = c("Cidade", "date")) %>%
      mutate(date = as.Date(Date, tz = "America/Sao_Paulo")) %>%
      filter(between(date, as.Date(input$start_date3), as.Date(input$end_date3))) %>%
      left_join(., thermo_localizacao, by = "Cidade")

    if (nrow(pol_data) == 0) return(leaflet() %>% addTiles())

    openairmaps::polarMap(pol_data,
                          pollutant = c("SO2", "NO2", "O3", "CO", "PM2.5", "PM10"),
                          latitude = "Latitude", longitude = "Longitude", key = TRUE, provider = "CartoDB.Positron")
  })

  output$wrose <- renderPlot({
    req(input$start_date3, input$end_date3)
    wr <- meteo %>%
      mutate(date = Date) %>%
      left_join(air_quality_data_ugm3, ., by = c("Cidade", "date")) %>%
      mutate(date = as.Date(date, tz = "America/Sao_Paulo")) %>%
      filter(between(date, as.Date(input$start_date3), as.Date(input$end_date3))) %>%
      left_join(., thermo_localizacao, by = "Cidade")
    if (nrow(wr) == 0) return(plot.new())
    openair::pollutionRose(wr, pollutant = "ws", type = "Cidade")
  })

  # ---------------------------------------------------------------------------
  # CONDITIONS PLOT (dist) - meteorological variables
  # ---------------------------------------------------------------------------
  output$dist <- renderPlot({
    req(input$start_date3, input$end_date3)
    by_hour <- meteo %>%
      mutate(date = Date) %>%
      left_join(air_quality_data_ugm3, ., by = c("Cidade", "date")) %>%
      mutate(date = as.Date(date, tz = "America/Sao_Paulo")) %>%
      filter(between(date, as.Date(input$start_date3), as.Date(input$end_date3))) %>%
      left_join(., thermo_localizacao, by = "Cidade")

    if (nrow(by_hour) == 0) return(plot.new())

    by_day <- by_hour %>%
      group_by(Cidade, date) %>%
      summarise(umid = mean(umid, na.rm = TRUE), .groups = "drop") %>%
      left_join(by_hour %>% select(Cidade, date, temp, prec), ., by = c("Cidade", "date")) %>%
      mutate(Date = ymd(date, tz = "America/Sao_Paulo") + hours(12))

    # choose scale for x axis
    total_days <- as.numeric(as.Date(input$end_date3) - as.Date(input$start_date3))
    x_breaks <- if (total_days < 45) "1 day" else "1 month"

    gg <- ggplot(by_hour, aes(x = Date, group = 1)) +
      geom_bar(aes(y = prec * 10, col = "Precipitação"), fill = "green", stat = "identity", alpha = 0.5) +
      geom_line(aes(x = by_day$Date, y = by_day$umid, col = "Umidade Relativa"), linewidth = 1) +
      geom_line(aes(y = temp, col = "Temperatura"), linewidth = 1) +
      geom_point(aes(x = by_day$Date, y = by_day$umid)) +
      geom_point(aes(y = temp)) +
      theme_bw() +
      facet_wrap(. ~ Cidade, scales = "free_y", ncol = 1) +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5), legend.position = "bottom") +
      scale_y_continuous(sec.axis = sec_axis(~./10, name = "Precipitação em mm")) +
      labs(x = "Dia de amostragem", y = "Temperatura (ºC) \n Umidade Relativa (%)", colour = "Variável") +
      scale_colour_manual(values = c("Precipitação" = "darkgreen", "Umidade Relativa" = "red", "Temperatura" = "blue")) +
      scale_x_datetime(labels = scales::label_date_short(), breaks = x_breaks)
    gg
  })

  # ---------------------------------------------------------------------------
  # RAW DATA DOWNLOAD and preview (table tab)
  # ---------------------------------------------------------------------------
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("DayData_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- air_quality_data_ugm3 %>%
        mutate(Date = as.Date(date, tz = "America/Sao_Paulo")) %>%
        filter(Date >= as.Date(input$start_date) & Date <= as.Date(input$end_date)) %>%
        mutate(Date = date) %>%
        select(date, Cidade:PM10)
      write_csv(df, file)
    }
  )

  output$tableData <- renderTable({
    df <- air_quality_data_ugm3 %>%
      mutate(Date = as.Date(date, tz = "America/Sao_Paulo")) %>%
      filter(Date >= as.Date(input$start_date) & Date <= as.Date(input$end_date)) %>%
      mutate(data = as.character(as_datetime(date, tz = "America/Sao_Paulo"))) %>%
      select(data, Cidade, SO2:PM10) %>%
      mutate(date = ifelse(str_detect(data, ":00"), as.character(data), paste0(as.character(data), " 00:00:00")),
             data = date) %>%
      select(-date)
    head(df, n = 20L)
  }, width = "70%")
}

# Run the application
shinyApp(ui = ui, server = server)
# --- FIM ---
# Para fins de demonstração, copie o script original dentro deste arquivo.
